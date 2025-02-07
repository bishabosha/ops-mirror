package serverlib

import mirrorops.ErrorAnnotation
import mirrorops.MetaAnnotation
import mirrorops.Operation
import mirrorops.OpsMirror
import mirrorops.VoidType
import serverlib.HttpService.Endpoints.Endpoint

import scala.NamedTuple.AnyNamedTuple
import scala.NamedTuple.NamedTuple

import quoted.*

trait HttpService[T]:
  val routes: Map[String, HttpService.Route]

object HttpService:
  import OpsMirror.*

  type Fields[N <: Tuple, O <: Tuple] =
    NamedTuple[N, Tuple.Map[O, OpToEndpoint]]

  type EncodeError[T] = T match
    case VoidType => Empty
    case _        => T

  type OpToEndpoint[Op] = Op match
    case OperationIns[ins] =>
      Op match
        case OperationOut[out] =>
          Op match
            case OperationErr[err] => Endpoint[ins, EncodeError[err], out]

  inline def derived[T](using m: OpsMirror.Of[T]): HttpService[T] = ${
    ServerMacros.derivedImpl[T]('m)
  }

  def endpoints[T: {HttpService as m, OpsMirror.Of as om}]: Endpoints[T] {
    type Fields = HttpService.Fields[om.MirroredOperationLabels, om.MirroredOperations]
  } =
    new Endpoints[T]:
      type Fields = HttpService.Fields[om.MirroredOperationLabels, om.MirroredOperations]
      def selectDynamic(name: String): HttpService.Route = m.routes(name)

  trait Endpoints[T] extends Selectable:
    type Fields <: AnyNamedTuple
    def selectDynamic(name: String): HttpService.Route

  object Endpoints:
    opaque type Endpoint[I, E, O] <: HttpService.Route = HttpService.Route

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
  end model

  case class Input(label: String, source: model.source)
  case class Route(route: model.method, inputs: Seq[Input])
end HttpService
