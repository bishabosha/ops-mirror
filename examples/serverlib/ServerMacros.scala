package serverlib

import scala.quoted.*
import mirrorops.{OpsMirror, Operation, VoidType}

import scala.util.chaining.given

import HttpService.{Route, Input, model, Empty}

object ServerMacros:
  import Endpoints.Endpoint

  type EncodeError[T] = T match
    case VoidType => Empty
    case _ => T

  def decorateImpl[T: Type](modelExpr: Expr[HttpService[T]], mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[Endpoints[T]] =
    import quotes.reflect.*

    def extractEndpoints[Ts: Type]: List[Type[?]] =
      OpsMirror.typesFromTuple[Ts].map:
        case '[op] => extractEndpoint[op]

    def extractEndpoint[T: Type]: Type[?] = Type.of[T] match
      case '[Operation { type InputTypes = inputTypes; type ErrorType = errorType; type OutputType = outputType }] =>
        Type.of[Endpoint[inputTypes, EncodeError[errorType], outputType]]

    val refinements = mirror match
      case '{
        $m: OpsMirror.Of[T] {
          type MirroredOperations = mirroredOps
          type MirroredOperationLabels = opLabels
        }
      } =>
        val labels = OpsMirror.stringsFromTuple[opLabels]
        labels.zip(extractEndpoints[mirroredOps]).foldLeft(Type.of[Endpoints[T]]: Type[?])({ (acc, p) =>
          ((acc, p): @unchecked) match
            case ('[acc], (l, '[e])) =>
              Refinement(TypeRepr.of[acc], l, TypeRepr.of[e]).asType
        })

    refinements match
      case '[resTpe] => '{ Endpoints($modelExpr).asInstanceOf[Endpoints[T] & resTpe] }
  end decorateImpl


  def derivedImpl[T: Type](mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[HttpService[T]] =
    import quotes.reflect.*

    def extractServices[Ts: Type]: List[Expr[Route]] = OpsMirror.typesFromTuple[Ts].map:
      case '[op] =>
      val metas = OpsMirror.metadata[op]
      val route = metas.base.collectFirst {
        case '{ $g: model.method } => g
      }
      val ins = Type.of[op] match
        case '[
          Operation { type InputLabels = inputLabels }
        ] =>
          val labels = OpsMirror.stringsFromTuple[inputLabels]
          val ins =
            labels.lazyZip(metas.inputs).map((l, ms) =>
              val method: Option[Expr[model.source]] = ms.collectFirst {
                case '{ $p: model.source } => p
              }
              '{Input(${Expr(l)}, ${method.getOrElse(report.errorAndAbort(s"expected a valid source for param ${l}"))})}
            )
            .pipe(Expr.ofSeq)
          ins

      route match
        case Some(r) => '{ Route($r, $ins) }
        case None => report.errorAndAbort(s"got the metadata elems ${metas.base.map(_.show)}")
    end extractServices

    val serviceExprs = mirror match
      case '{
        type opLabels <: Tuple
        $m: OpsMirror.Of[T] {
          type MirroredOperations = mirroredOps
          type MirroredOperationLabels = opLabels
        }
      } =>
        val labels = OpsMirror.stringsFromTuple[opLabels]
        labels.groupBy(identity).values.find(_.sizeIs > 1).foreach: label =>
          report.errorAndAbort(s"HttpService does not support overloaded methods, found ${label.head} more than once.")
        labels.zip(extractServices[mirroredOps]).map((l, s) => '{(${Expr(l)}, $s)})
    ('{
      new HttpService[T] {
        val routes = Map(${Varargs(serviceExprs)}*)
      }
    })
  end derivedImpl
