package serverlib.jdkhttp

import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpClient.Version
import java.net.http.HttpClient.Redirect
import java.net.http.HttpResponse
import java.net.URI

import serverlib.HttpService.Endpoints.Endpoint
import serverlib.HttpService.Empty
import serverlib.HttpService.Input
import serverlib.HttpService.model.source
import serverlib.HttpService.model.method

import PartialRequest.{Request, Bundler, Func}

class PartialRequest[I <: Tuple, E, O] private (
    e: Endpoint[I, E, O],
    baseURI: String,
    builder: HttpRequest.Builder):

  private val optBody: Option[IArray[String] => String] =
    e.inputs.view
      .map(_.source)
      .zipWithIndex
      .collectFirst({ case (source.body(), i) => bundle => bundle(i) })

  private val (httpMethod, route) = e.route match
    case method.get(route)  => ("GET", route)
    case method.post(route) => ("POST", route)
    case method.put(route)  => ("PUT", route)

  private val uriParts: Seq[IArray[String] => String] =
    val uriParams: Map[String, Int] =
      e.inputs.zipWithIndex
        .collect({ case (Input(label, source.path()), i) =>
          (label, i)
        })
        .toMap
    Server
      .uriPattern(route)
      .map({
        case Server.UriParts.Exact(str) => Function.const(str)
        case Server.UriParts.Wildcard(name) =>
          val i = uriParams(name)
          bundle => bundle(i)
      })
  end uriParts

  protected def handle(bundle: IArray[String]): Request[E, O] =
    val uri     = uriParts.view.map(_(bundle)).mkString(baseURI, "/", "")
    val withUri = builder.uri(URI.create(uri))
    val withBody = optBody.fold(withUri)(get =>
      val body = get(bundle)
      assert(body.nonEmpty, "empty body")
      withUri
        .setHeader("Content-Type", "text/plain")
        .method(httpMethod, HttpRequest.BodyPublishers.ofString(body))
    )
    Request(withBody.build())
  end handle

  inline def prepare: Func[I, Request[E, O]] = summon[Bundler[I, E, O]].bundle(this)
end PartialRequest

object PartialRequest:

  class Request[E, O] private[PartialRequest] (req: HttpRequest):
    protected def baseSend =
      HttpClient.newHttpClient().send(req, HttpResponse.BodyHandlers.ofString())

    def sendWithError()(using Des[E], Des[O]): Either[E, O] =
      val response = baseSend
      val body     = response.body()
      if response.statusCode() < 400 then Right(summon[Des[O]].deserialize(body))
      else Left(summon[Des[E]].deserialize(body))
    end sendWithError

    def sendNoError()(using Des[O]): O =
      val response = baseSend
      if response.statusCode() < 400 then summon[Des[O]].deserialize(response.body())
      else
        throw new RuntimeException(
          s"Request failed with status ${response.statusCode()} and body ${response.body()}"
        )
      end if
    end sendNoError

    inline def send()(using Des[E], Des[O]): Res[E, O] = inline compiletime.erasedValue[E] match
      case _: Empty => sendNoError()
      case _        => sendWithError()
  end Request

  type Func[I <: Tuple, O] = I match
    case EmptyTuple      => () => O
    case a *: EmptyTuple => a => O
    case (a, b)          => (a, b) => O

  type Res[E, O] = E match
    case Empty => O
    case _     => Either[E, O]

  trait Bundler[I <: Tuple, E, O]:
    inline def bundle(e: PartialRequest[I, E, O]): Func[I, Request[E, O]]

  trait Des[T]:
    def deserialize(s: String): T

  object Des:
    given Des[String] with
      def deserialize(s: String): String = s
    given Des[Int] with
      def deserialize(s: String): Int = s.toInt

    given Des[Unit] with
      def deserialize(s: String): Unit = ()

    given Des[Empty] with
      def deserialize(s: String): Empty = ??? // should never be called
  end Des

  trait Ser[I]:
    def serialize(i: I): String

  object Ser:
    given Ser[String] with
      def serialize(i: String): String = i
    given Ser[Int] with
      def serialize(i: Int): String = i.toString
  end Ser

  object Bundler:
    given [I <: Tuple, E, O]: Bundler[I, E, O] with
      inline def bundle(req: PartialRequest[I, E, O]): Func[I, Request[E, O]] =
        inline compiletime.erasedValue[I] match
          case _: EmptyTuple => () => req.handle(IArray.empty)
          case _: (a *: EmptyTuple) =>
            (a0: a) => req.handle(IArray(compiletime.summonInline[Ser[a]].serialize(a0)))
          case _: (a, b) =>
            (a0: a, a1: b) =>
              val bundle = IArray(
                compiletime.summonInline[Ser[a]].serialize(a0),
                compiletime.summonInline[Ser[b]].serialize(a1)
              )
              req.handle(bundle)
    end given
  end Bundler

  def apply[I <: Tuple, E, O](e: Endpoint[I, E, O], baseURI: String)(using Bundler[I, E, O])
      : PartialRequest[I, E, O] =
    new PartialRequest(e, s"$baseURI/", HttpRequest.newBuilder())
end PartialRequest
