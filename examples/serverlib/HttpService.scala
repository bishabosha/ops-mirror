package serverlib

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple

import quoted.*
import mirrorops.{OpsMirror, MetaAnnotation, ErrorAnnotation, Operation}

trait HttpService[T]:
  val routes: Map[String, HttpService.Route]

object HttpService:
  import OpsMirror.*

  type Fields[Ops <: OpsMirror, F[I <: Tuple, E, O]] <: AnyNamedTuple = (Ops, Ops) match
    case (OpsMirrorNs[ns], OpsMirrorOps[ops]) =>
      NamedTuple[ns, Fields1[ops, F]]

  type Fields1[Ops <: Tuple, F[I <: Tuple, E, O]] <: Tuple = Ops match
    case EmptyTuple => EmptyTuple
    case op *: ops => OpToEndpoint[op, F] *: Fields1[ops, F]

  type OpToEndpoint[Op <: Operation, F[I <: Tuple, E, O]] = Op match
    case OperationIns[ins] => Op match
      case OperationOut[out] => Op match
        case OperationErr[err] => F[ins, ServerMacros.EncodeError[err], out]


  inline def derived[T](using m: OpsMirror.Of[T]): HttpService[T] = ${ ServerMacros.derivedImpl[T]('m) }

  // transparent inline def endpoints[T](using m: HttpService[T], om: OpsMirror.Of[T]): Endpoints[T] =
  //   ${ ServerMacros.decorateImpl[T]('m, 'om) }

  transparent inline def endpoints[T](using m: HttpService[T], om: OpsMirror.Of[T]): Endpoints2[T] =
    ${ ServerMacros.decorateImpl2[T]('m, 'om) }

  // transparent inline def labels[T](using om: OpsMirror.Of[T]): List[String] =
  //   ${ ServerMacros.labelsImpl[T]('om) }

  // final class Endpoints[T](val model: HttpService[T]) extends Selectable:
  //   def selectDynamic(name: String): HttpService.Route = model.routes(name)

  class Endpoints2[T](val model: HttpService[T]) extends Selectable:
    type Fields <: AnyNamedTuple
    def selectDynamic(name: String): HttpService.Route = model.routes(name)

  final class EndpointsFinal[T, Fs <: AnyNamedTuple](m: HttpService[T]) extends Endpoints2[T](m):
    override type Fields = Fs

  object Endpoints:
    opaque type Endpoint[+N, I, E, O] <: HttpService.Route = HttpService.Route

  sealed trait Empty

  object model:
    class failsWith[E] extends ErrorAnnotation[E]

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
