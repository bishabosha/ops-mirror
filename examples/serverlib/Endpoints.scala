package serverlib

import mirrorops.OpsMirror

final class Endpoints[T](val model: HttpService[T]) extends Selectable:
  def selectDynamic(name: String): HttpService.Route[?, ?] = model.routes(name)

object Endpoints:

  opaque type Endpoint[I, O] <: HttpService.Route[I, O] = HttpService.Route[I, O]

  transparent inline def of[T](using m: HttpService[T], om: OpsMirror.Of[T]): Endpoints[T] =
    ${ ServerMacros.decorateImpl[T]('m, 'om) }
