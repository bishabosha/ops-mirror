package serverlib

import quoted.*
import mirrorops.{OpsMirror, MetaAnnotation, ErrorAnnotation}

trait HttpService[T]:
  val routes: Map[String, HttpService.Route]

object HttpService:
  inline def derived[T](using m: OpsMirror.Of[T]): HttpService[T] = ${ ServerMacros.derivedImpl[T]('m) }

  transparent inline def endpoints[T](using m: HttpService[T], om: OpsMirror.Of[T]): Endpoints[T] =
    ${ ServerMacros.decorateImpl[T]('m, 'om) }

  final class Endpoints[T](val model: HttpService[T]) extends Selectable:
    def selectDynamic(name: String): HttpService.Route = model.routes(name)

  object Endpoints:
    opaque type Endpoint[I, E, O] <: HttpService.Route = HttpService.Route

  sealed trait Empty

  object model:
    class fail[E] extends ErrorAnnotation[E]

    enum method extends MetaAnnotation:
      case get(route: String)
      case post(route: String)
      case put(route: String)

    enum source extends MetaAnnotation:
      case path()
      case query()
      case body()

  case class Input(label: String, source: model.source)
  case class Route(route: model.method, inputs: Seq[Input])
