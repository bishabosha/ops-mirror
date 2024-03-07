package serverlib.jdkhttp

import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer

import serverlib.HttpService.Empty
import serverlib.Endpoints.Endpoint
import scala.util.TupledFunction

import serverlib.util.optional
import scala.collection.View.Empty

class Server private (private val internal: HttpServer) extends AutoCloseable:
  def close(): Unit = internal.stop(0)

object Server:

  enum HttpMethod:
    case Get, Post, Put

  type UriHandler = String => Option[Map[String, String]]

  type Func[I <: Tuple, E, O] = I match
    case EmptyTuple => () => Res[E, O]
    case a *: EmptyTuple => a => Res[E, O]
    case (a, b) => (a, b) => Res[E, O]

  type Res[E, O] = E match
    case Empty => O
    case _ => Either[E, O]

  trait Exchanger[I <: Tuple, E, O](using Ser[Res[E, O]]):
    def apply(bundle: Bundle, func: Func[I, E, O]): Ser.Result

  trait Ser[O]:
    def serialize(o: O): Ser.Result

  object Ser:
    type Result = Either[Option[Array[Byte]], Option[Array[Byte]]]

    given [E: Ser, O: Ser]: Ser[Either[E, O]] with
      def serialize(o: Either[E, O]): Result = o.fold(
        e => Left(summon[Ser[E]].serialize(e).merge),
        o => Right(summon[Ser[O]].serialize(o).merge)
      )

    given Ser[Unit] with
      def serialize(o: Unit): Result = Right(None)

    given Ser[String] with
      def serialize(o: String): Result = Right(Some(o.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

    given Ser[Int] with
      def serialize(o: Int): Result = Right(Some(o.toString.getBytes(java.nio.charset.StandardCharsets.UTF_8)))

  trait Des[I]:
    def deserialize(str: String): I

  object Des:
    given Des[Int] with
      def deserialize(str: String): Int = str.toInt

    given Des[String] with
      def deserialize(str: String): String = str

  object Exchanger:
    inline given [I <: Tuple, E, O](using Ser[Res[E, O]]): Exchanger[I, E, O] = new Exchanger[I, E, O]:
      def apply(bundle: Bundle, func: Func[I, E, O]): Ser.Result =
        val res =
          inline compiletime.erasedValue[I] match
            case _: EmptyTuple => func.asInstanceOf[() => Res[E, O]]()
            case _: (a *: EmptyTuple) =>
              val dA = compiletime.summonInline[Des[a]]
              func.asInstanceOf[a => Res[E, O]](dA.deserialize(bundle.arg(0)))
            case _: (a, b) =>
              val dA = compiletime.summonInline[Des[a]]
              val dB = compiletime.summonInline[Des[b]]
              func.asInstanceOf[(a, b) => Res[E, O]](dA.deserialize(bundle.arg(0)), dB.deserialize(bundle.arg(1)))
        summon[Ser[Res[E, O]]].serialize(res)

  trait Bundle:
    def arg(index: Int): String

  extension [I <: Tuple, E, O](e: Endpoint[I, E, O])
    def handle(op: Func[I, E, O])(using Exchanger[I, E, O]): Handler[I, E, O] =
      Handler[I, E, O](e, op, summon[Exchanger[I, E, O]])

  private def rootHandler(handlers: List[Handler[?, ?, ?]]): HttpHandler =
    val lazyHandlers = handlers.to(LazyList)
      .map: h =>
        val (method, uriHandler) = h.route
        method -> (uriHandler, h)
      .groupMap:
        (method, _) => method
      .apply:
        (_, pair) => pair

    (exchange: HttpExchange) =>
      // get method
      val method = exchange.getRequestMethod match
        case "GET" => HttpMethod.Get
        case "POST" => HttpMethod.Post
        case "PUT" => HttpMethod.Put
        case _ => throw new IllegalArgumentException("Unsupported method")

      // get uri
      val uri = exchange.getRequestURI.getPath()
      // match the uri to a handler
      val handlerOpt = lazyHandlers.get(method).flatMap: ls =>
        ls
          .flatMap: (uriHandler, h) =>
            uriHandler(uri).map: params =>
              h -> params
          .headOption

      def readBody(): Array[Byte] =
        // consume the full input stream
        val is = exchange.getRequestBody
        try {
          val bis = new java.io.BufferedInputStream(is)
          try {
            var initial = bis.available()
            var buf = new Array[Byte](initial)
            bis.read(buf)
            var estimated = bis.available()
            while estimated > 0 do
              val newBuf = new Array[Byte](buf.length + estimated)
              System.arraycopy(buf, 0, newBuf, 0, buf.length)
              bis.read(newBuf, buf.length, estimated)
              estimated = bis.available()
              buf = newBuf
            buf
          } finally bis.close()
        } finally is.close()

      try handlerOpt match
        case None =>
          exchange.sendResponseHeaders(404, -1)
        case Some((handler, params)) =>
          val body = readBody()
          val bodyStr = new String(body, java.nio.charset.StandardCharsets.UTF_8)
          println(s"matched ${uri} to handler ${handler.debug} with params ${params}\nbody: ${bodyStr}")

          handler.exchange(params, bodyStr) match
            case Left(errExchange) =>
              // TODO: in real world you would encode the data type to the error format
              errExchange match
                case None =>
                  exchange.sendResponseHeaders(500, -1)
                case Some(response) =>
                  exchange.sendResponseHeaders(500, response.length)
                  exchange.getResponseBody.write(response)
            case Right(response) =>
              response match
                case None =>
                  exchange.sendResponseHeaders(200, -1)
                case Some(response) =>
                  exchange.sendResponseHeaders(200, response.length)
                  exchange.getResponseBody.write(response)
      finally
        exchange.close()

  class Handler[I <: Tuple, E, O](e: Endpoint[I, E, O], op: Func[I, E, O], exchange: Exchanger[I, E, O]):
    import serverlib.HttpService.model.*
    import serverlib.HttpService.Tag

    type Bundler = (params: Map[String, String], body: String) => Bundle
    type BundleArg = (params: Map[String, String], body: String) => String

    val template: Bundler =
      val readers: Array[BundleArg] = e.inputs
        .map[BundleArg]: i =>
          (i.source: @unchecked) match
            case source.path() =>
              val name = i.label
              (params, _) => params(name)
            case source.body() =>
              (_, body) => body
        .toArray
      (params, body) =>
        new:
          def arg(index: Int): String = readers(index)(params, body)

    def exchange(params: Map[String, String], body: String): Ser.Result =
      val bundle = template(params, body)
      exchange(bundle, op)

    def uriHandle(route: String): UriHandler =
      enum UriParts:
        case Exact(str: String)
        case Wildcard(name: String)
      assert(route.startsWith("/"))
      val parts = route.split("/").view.drop(1)
      assert(parts.forall(_.nonEmpty))
      val elems = parts.toIndexedSeq.map {
        case s if s.startsWith("{") && s.endsWith("}") => UriParts.Wildcard(s.slice(1, s.length - 1))
        case s => UriParts.Exact(s)
      }
      uri => optional:
        val uriElems = uri.split("/")
        val elemsIt = elems.iterator
        val uriIt = uriElems.iterator.filter(_.nonEmpty)
        var result = Map.empty[String, String]
        while elemsIt.hasNext && uriIt.hasNext do
          elemsIt.next() match
            case UriParts.Exact(str) =>
              if uriIt.next() != str then optional.abort
            case UriParts.Wildcard(name) =>
              result += (name -> uriIt.next())
        if elemsIt.hasNext || uriIt.hasNext then optional.abort
        result

    def debug: String = e.route match
      case method.get(route) => s"GET ${route}"
      case method.post(route) => s"POST ${route}"
      case method.put(route) => s"PUT ${route}"

    def route: (HttpMethod, UriHandler) = e.route match
      case method.get(route) => (HttpMethod.Get, uriHandle(route))
      case method.post(route) => (HttpMethod.Post, uriHandle(route))
      case method.put(route) => (HttpMethod.Put, uriHandle(route))

  class ServerBuilder():
    private val handlers: ListBuffer[Handler[?, ?, ?]] = ListBuffer()

    def addEndpoint[I <: Tuple, E, O](handler: Handler[I, E, O]): this.type =
      handlers += handler
      this

    def create(): Server =
      val server = HttpServer.create()
      val handlers0 = handlers.toList
      server.bind(new java.net.InetSocketAddress(8080), 0)
      val _ = server.createContext("/", rootHandler(handlers0))
      server.setExecutor(Executors.newVirtualThreadPerTaskExecutor())
      server.start()
      Server(server)
