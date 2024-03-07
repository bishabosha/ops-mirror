package serverlib

import scala.quoted.*
import mirrorops.{OpsMirror, Operation}

import scala.util.chaining.given

import HttpService.{Route, Tag, Input, model}

object ServerMacros:
  import Endpoints.Endpoint

  def extractLabels[Ts: Type](using Quotes): List[String] = Type.of[Ts] match
    case '[op *: ts] => extractLabel[op] :: extractLabels[ts]
    case '[EmptyTuple] => Nil

  def extractLabel[T: Type](using Quotes): String =
    import quotes.reflect.*
    TypeRepr.of[T] match
      case ConstantType(StringConstant(label)) => label


  def decorateImpl[T: Type](modelExpr: Expr[HttpService[T]], mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[Endpoints[T]] =
    import quotes.reflect.*

    def extractEndpoints[Ts: Type]: List[Type[?]] = Type.of[Ts] match
      case '[op *: ts] => extractEndpoint[op] :: extractEndpoints[ts]
      case '[EmptyTuple] => Nil

    def extractEndpoint[T: Type]: Type[?] = Type.of[T] match
      case '[Operation { type InputTypes = inputTypes; type OutputType = outputType }] =>
        Type.of[Endpoint[inputTypes, outputType]]

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

    def extractInputs[Ts: Type]: List[Expr[Tag[?]]] = Type.of[Ts] match
      case '[op *: ts] => extractInput[op] :: extractInputs[ts]
      case '[EmptyTuple] => Nil

    def extractInput[T: Type]: Expr[Tag[?]] = Type.of[T] match
      case '[String] => '{ Tag.String }
      case '[Unit] => '{ Tag.Unit }
      case '[Int] => '{ Tag.Int }
      case _ => report.errorAndAbort(s"can not generate a Tag for type ${Type.show[T]}")

    def extractServices[Ts: Type]: List[Expr[Route[?, ?]]] = Type.of[Ts] match
      case '[op *: ts] => extractService[op] :: extractServices[ts]
      case '[EmptyTuple] => Nil

    def extractService[Op: Type]: Expr[Route[?, ?]] =
      val metas = OpsMirror.metadata[Op]
      val route = metas.base.collectFirst {
        case '{ $g: model.method } => g
      }
      val (inTpes, ins) = Type.of[Op] match
        case '[Operation { type InputTypes = inputTypes; type InputLabels = inputLabels }] =>
          val labels = extractLabels[inputLabels]
          val ins =
            extractInputs[inputTypes].lazyZip(labels).lazyZip(metas.inputs).map((t, l, ms) =>
              val method: Option[Expr[model.source]] = ms.collectFirst {
                case '{ $p: model.source } => p
              }
              '{Input(${Expr(l)}, $t, ${method.getOrElse(report.errorAndAbort(s"expected a valid source for param ${l}"))})}
            )
            .pipe(Expr.ofSeq)
          (Type.of[inputTypes], ins)

      val (outTpe, out) = Type.of[Op] match
        case '[Operation { type OutputType = outputType }] =>
          (Type.of[outputType], extractInput[outputType])

      route match
        case Some(r) =>
          (inTpes, outTpe) match
            case ('[i], '[o]) => '{ Route[i, o]($r, $ins, $out) }
        case None => report.errorAndAbort(s"got the metadata elems ${metas.base.map(_.show)}")
    end extractService

    val servicesExpr = mirror match
      case '{
        type opLabels <: Tuple
        $m: OpsMirror.Of[T] {
          type MirroredOperations = mirroredOps
          type MirroredOperationLabels = opLabels
        }
      } =>
        val labels = extractLabels[opLabels]
        Expr.ofList(labels.zip(extractServices[mirroredOps]).map((l, s) => '{(${Expr(l)}, $s)}))
    ('{
      new HttpService[T] {
        val routes = Map.from($servicesExpr)
      }
    })
  end derivedImpl
