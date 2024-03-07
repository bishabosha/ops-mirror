package serverlib

import scala.quoted.*
import mirrorops.{OpsMirror, Operation, VoidType}

import scala.util.chaining.given

import HttpService.{Route, Tag, Input, model, Empty}

object ServerMacros:
  import Endpoints.Endpoint

  def extractLabels[Ts: Type](using Quotes): List[String] =
    import quotes.reflect.*
    OpsMirror.typesFromTuple[Ts].map:
      case '[t] => TypeRepr.of[t] match
        case ConstantType(StringConstant(label)) => label

  def encodeError[T: Type](using Quotes): Type[?] = Type.of[T] match
    case '[VoidType] => Type.of[Empty]
    case tpe => tpe

  def decorateImpl[T: Type](modelExpr: Expr[HttpService[T]], mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[Endpoints[T]] =
    import quotes.reflect.*

    def extractEndpoints[Ts: Type]: List[Type[?]] = Type.of[Ts] match
      case '[op *: ts] => extractEndpoint[op] :: extractEndpoints[ts]
      case '[EmptyTuple] => Nil

    def extractEndpoint[T: Type]: Type[?] = Type.of[T] match
      case '[Operation { type InputTypes = inputTypes; type ErrorType = errorType; type OutputType = outputType }] =>
        encodeError[errorType] match
          case '[e] => Type.of[Endpoint[inputTypes, e, outputType]]

    val refinements = mirror match
      case '{
        type opLabels <: Tuple
        $m: OpsMirror.Of[T] {
          type MirroredOperations = mirroredOps
          type MirroredOperationLabels = opLabels
        }
      } =>
        val labels = extractLabels[opLabels]
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

    def reifyTypes[Ts: Type]: List[Expr[Tag[?]]] =
      OpsMirror.typesFromTuple[Ts].map:
        case '[t] => reifyType[t]

    def reifyType[T: Type]: Expr[Tag[?]] = Type.of[T] match
      case '[String] => '{ Tag.String }
      case '[Unit] => '{ Tag.Unit }
      case '[Int] => '{ Tag.Int }
      case '[Empty] => '{ Tag.Empty }
      case _ => report.errorAndAbort(s"can not generate a Tag for type ${Type.show[T]}")

    def extractServices[Ts: Type]: List[Expr[Route[?, ?, ?]]] = OpsMirror.typesFromTuple[Ts].map:
      case '[op] =>
      val metas = OpsMirror.metadata[op]
      val route = metas.base.collectFirst {
        case '{ $g: model.method } => g
      }
      val (inTpes, ins) = Type.of[op] match
        case '[Operation { type InputTypes = inputTypes; type InputLabels = inputLabels }] =>
          val labels = extractLabels[inputLabels]
          val ins =
            reifyTypes[inputTypes].lazyZip(labels).lazyZip(metas.inputs).map((t, l, ms) =>
              val method: Option[Expr[model.source]] = ms.collectFirst {
                case '{ $p: model.source } => p
              }
              '{Input(${Expr(l)}, $t, ${method.getOrElse(report.errorAndAbort(s"expected a valid source for param ${l}"))})}
            )
            .pipe(Expr.ofSeq)
          (Type.of[inputTypes], ins)

      val (outTpe, out) = Type.of[op] match
        case '[Operation { type OutputType = outputType }] =>
          (Type.of[outputType], reifyType[outputType])

      val (errTpe, err) = Type.of[op] match
        case '[Operation { type ErrorType = errorType }] =>
          encodeError[errorType] match
            case '[e] => (Type.of[e], reifyType[e])

      route match
        case Some(r) =>
          (inTpes, errTpe, outTpe) match
            case ('[i], '[e], '[o]) => '{ Route[i, e, o]($r, $ins, $err, $out) }
        case None => report.errorAndAbort(s"got the metadata elems ${metas.base.map(_.show)}")
    end extractServices

    val servicesExpr = mirror match
      case '{
        type opLabels <: Tuple
        $m: OpsMirror.Of[T] {
          type MirroredOperations = mirroredOps
          type MirroredOperationLabels = opLabels
        }
      } =>
        val labels = extractLabels[opLabels]
        labels.groupBy(identity).values.find(_.sizeIs > 1).foreach: label =>
          report.errorAndAbort(s"HttpService does not support overloaded methods, found ${label.head} more than once.")
        Expr.ofList(labels.zip(extractServices[mirroredOps]).map((l, s) => '{(${Expr(l)}, $s)}))
    ('{
      new HttpService[T] {
        val routes = Map.from($servicesExpr)
      }
    })
  end derivedImpl
