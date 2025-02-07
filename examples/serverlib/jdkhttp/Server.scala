package serverlib.jdkhttp

import language.experimental.namedTuples

import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpExchange
import com.sun.net.httpserver.HttpServer

import java.util.concurrent.Executors
import scala.collection.mutable.ListBuffer

import serverlib.HttpService.Empty
import serverlib.HttpService.Endpoints
import serverlib.HttpService.Endpoints.Endpoint
import scala.util.TupledFunction

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple

import serverlib.util.optional
import scala.collection.View.Empty
import mirrorops.OpsMirror
import serverlib.HttpService
import serverlib.HttpService.Route

class Server private (private val internal: HttpServer) extends AutoCloseable:
  def close(): Unit = internal.stop(0)

object Server:

  enum UriParts:
    case Exact(str: String)
    case Wildcard(name: String)

  def uriPattern(route: String): IndexedSeq[UriParts] =
    assert(route.startsWith("/"))
    val parts = route.split("/").view.drop(1)
    assert(parts.forall(_.nonEmpty))
    parts.toIndexedSeq.map {
      case s if s.startsWith("{") && s.endsWith("}") => UriParts.Wildcard(s.slice(1, s.length - 1))
      case s                                         => UriParts.Exact(s)
    }
  end uriPattern

  enum HttpMethod:
    case Get, Post, Put

  type UriHandler = String => Option[Map[String, String]]

  type Func[I <: Tuple, E, O] = I match
    case EmptyTuple      => () => Res[E, O]
    case a *: EmptyTuple => a => Res[E, O]
    case (a, b)          => (a, b) => Res[E, O]

  type Res[E, O] = E match
    case Empty => O
    case _     => Either[E, O]

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
    end given

    given Ser[Unit] with
      def serialize(o: Unit): Result = Right(None)

    given Ser[String] with
      def serialize(o: String): Result = Right(
        Some(o.getBytes(java.nio.charset.StandardCharsets.UTF_8))
      )
    end given

    given Ser[Int] with
      def serialize(o: Int): Result = summon[Ser[String]].serialize(o.toString)
  end Ser

  trait Des[I]:
    def deserialize(str: String): I

  object Des:
    given Des[Int] with
      def deserialize(str: String): Int = str.toInt

    given Des[String] with
      def deserialize(str: String): String = str
  end Des

  object Exchanger:
    @annotation.nowarn("msg=New anonymous class definition will be duplicated at each inline site")
    inline given [I <: Tuple, E, O](using Ser[Res[E, O]]): Exchanger[I, E, O] =
      new Exchanger[I, E, O]:
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
                func.asInstanceOf[(a, b) => Res[E, O]](
                  dA.deserialize(bundle.arg(0)),
                  dB.deserialize(bundle.arg(1))
                )
          summon[Ser[Res[E, O]]].serialize(res)
        end apply
  end Exchanger

  trait Bundle:
    def arg(index: Int): String

  type Handlers[I <: AnyNamedTuple] =
    NamedTuple.Map[
      I,
      [X] =>> X match
        case Endpoint[ins, err, out] => Handler[ins, err, out]
    ]

  type HandlerFuncs[I <: AnyNamedTuple] =
    NamedTuple.From[
      NamedTuple.Map[
        I,
        [X] =>> X match
          case Endpoint[ins, err, out] => Func[ins, err, out]
      ]
    ]

  type HandlerExchangers[I <: AnyNamedTuple] =
    Tuple.Map[
      NamedTuple.DropNames[I],
      [X] =>> X match
        case Endpoint[ins, err, out] => Exchanger[ins, err, out]
    ]

  private def rootHandler(handlers: List[Handler[?, ?, ?]]): HttpHandler =
    val lazyHandlers = handlers
      .to(LazyList)
      .map: h =>
        val (method, uriHandler) = h.route
        method -> (uriHandler, h)
      .groupMap: (method, _) =>
        method
      .apply: (_, pair) =>
        pair

    def makeExchange(exchange: HttpExchange) =
      // get method
      val method = exchange.getRequestMethod match
        case "GET"  => HttpMethod.Get
        case "POST" => HttpMethod.Post
        case "PUT"  => HttpMethod.Put
        case _      => throw new IllegalArgumentException("Unsupported method")

      // get uri
      val uri = exchange.getRequestURI.getPath()
      // match the uri to a handler
      val handlerOpt = lazyHandlers
        .get(method)
        .flatMap: ls =>
          ls
            .flatMap: (uriHandler, h) =>
              uriHandler(uri).map: params =>
                h -> params
            .headOption

      def readBody(length: Int): Array[Byte] =
        // consume the full input stream
        val is = exchange.getRequestBody
        try
          is.readAllBytes().ensuring(_.length == length, "read less bytes than expected")
        finally
          is.close()
      end readBody

      try
        handlerOpt match
          case None =>
            exchange.sendResponseHeaders(404, -1)
          case Some((handler, params)) =>
            val length = Option(exchange.getRequestHeaders.getFirst("Content-Length"))
              .map(_.toInt)
              .getOrElse(0)
            val body    = readBody(length)
            val bodyStr = new String(body, java.nio.charset.StandardCharsets.UTF_8)
            println(
              s"matched ${uri} to handler ${handler.debug} with params ${params}\nbody: ${bodyStr}"
            )

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
            end match
      finally
        exchange.close()
      end try
    end makeExchange

    makeExchange(_)
  end rootHandler

  class Handler[I <: Tuple, E, O](
      routeName: String,
      e: Endpoint[I, E, O],
      op: Func[I, E, O],
      exchange: Exchanger[I, E, O]):
    import serverlib.HttpService.model.*

    type Bundler   = (params: Map[String, String], body: String) => Bundle
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
    end template

    def exchange(params: Map[String, String], body: String): Ser.Result =
      val bundle = template(params, body)
      exchange(bundle, op)

    def uriHandle(route: String): UriHandler =
      val elems = uriPattern(route)
      uri =>
        optional:
          val uriElems = uri.split("/")
          val elemsIt  = elems.iterator
          val uriIt    = uriElems.iterator.filter(_.nonEmpty)
          var result   = Map.empty[String, String]
          while elemsIt.hasNext && uriIt.hasNext do
            elemsIt.next() match
              case UriParts.Exact(str) =>
                if uriIt.next() != str then optional.abort
              case UriParts.Wildcard(name) =>
                result += (name -> uriIt.next())
          end while
          if elemsIt.hasNext || uriIt.hasNext then optional.abort
          result
    end uriHandle

    def debug: String = e.route match
      case method.get(route)  => s"$routeName: GET ${route}"
      case method.post(route) => s"$routeName: POST ${route}"
      case method.put(route)  => s"$routeName: PUT ${route}"

    def route: (HttpMethod, UriHandler) = e.route match
      case method.get(route)  => (HttpMethod.Get, uriHandle(route))
      case method.post(route) => (HttpMethod.Post, uriHandle(route))
      case method.put(route)  => (HttpMethod.Put, uriHandle(route))

  end Handler

  class ServerBuilder():
    private val handlers: ListBuffer[Handler[?, ?, ?]] = ListBuffer()

    transparent inline def addEndpoints[Service, I <: AnyNamedTuple](
        e: Endpoints[Service] { type Fields = I }
      )(impls: HandlerFuncs[I]
      ): this.type =
      val namesTuple = compiletime.constValueTuple[NamedTuple.Names[I]]
      val exchangers = compiletime.summonAll[HandlerExchangers[I]]
      type N0 <: String
      type I0 <: Tuple
      type E0
      type O0
      val handles = impls
        .asInstanceOf[Product]
        .productIterator
        .zip(namesTuple.productIterator.asInstanceOf[Iterator[N0]])
        .zip(exchangers.productIterator)
        .map({ case ((func, name), exchanger) =>
          val endpoint =
            e.selectDynamic(name).asInstanceOf[Endpoint[I0, E0, O0]]
          val op = func.asInstanceOf[Func[I0, E0, O0]]
          val ex = exchanger.asInstanceOf[Exchanger[I0, E0, O0]]
          Handler(name, endpoint, op, ex)
        })
        .toList

      // Log the added handlers for debugging
      println(s"adding handles ${handles.map(_.debug).mkString("\n> ", "\n> ", "")}")
      handlers ++= handles
      this
    end addEndpoints

    def create(port: Int): Server =
      val server    = HttpServer.create()
      val handlers0 = handlers.toList
      server.bind(new java.net.InetSocketAddress(port), 0)
      val _ = server.createContext("/", rootHandler(handlers0))
      server.setExecutor(Executors.newVirtualThreadPerTaskExecutor())
      server.start()
      Server(server)
    end create
  end ServerBuilder
end Server
