package mirrorops

import quoted.*

import scala.util.chaining.given
import scala.annotation.implicitNotFound

@implicitNotFound("No OpsMirror could be generated.\nDiagnose any issues by calling OpsMirror.reify[T] directly")
sealed trait OpsMirror:
  type MirroredType
  type MirroredLabel
  type MirroredOperations
  type MirroredOperationLabels

sealed trait Meta

sealed trait VoidType

open class MetaAnnotation extends scala.annotation.RefiningAnnotation

sealed trait Operation:
  type Metadata <: Tuple
  type InputTypes <: Tuple
  type InputLabels <: Tuple
  type InputMetadatas <: Tuple
  type ErrorType
  type OutputType

object OpsMirror:
  type Of[T] = OpsMirror { type MirroredType = T }

  transparent inline given reify[T]: Of[T] = ${ reifyImpl[T] }

  case class Metadata(base: List[Expr[Any]], inputs: List[List[Expr[Any]]])

  def typesFromTuple[Ts: Type](using Quotes): List[Type[?]] =
    Type.of[Ts] match
      case '[t *: ts] => Type.of[t] :: typesFromTuple[ts]
      case '[EmptyTuple] => Nil

  def typesToTuple(list: List[Type[?]])(using Quotes): Type[?] = list match
    case '[t] :: ts => typesToTuple(ts) match
      case '[type ts <: Tuple; ts] => Type.of[t *: ts]
    case _ => Type.of[EmptyTuple]

  def metadata[Op: Type](using Quotes): Metadata =
    import quotes.reflect.*

    def extractMetass[Metadatas: Type]: List[List[Expr[Any]]] =
      typesFromTuple[Metadatas].map:
        case '[m] => extractMetas[m]

    def extractMetas[Metadata: Type]: List[Expr[Any]] =
      typesFromTuple[Metadata].map:
        case '[m] => TypeRepr.of[m] match
          case AnnotatedType(_, annot) =>
            annot.asExpr
          case tpe =>
            report.errorAndAbort(s"got the metadata element ${tpe.show}")

    Type.of[Op] match
      case '[Operation {
        type Metadata = metadata
        type InputMetadatas = inputMetadatas
      }] => Metadata(extractMetas[metadata], extractMetass[inputMetadatas])
      case _ => report.errorAndAbort("expected an Operation with Metadata.")

  private def reifyImpl[T: Type](using Quotes): Expr[Of[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val cls = tpe.classSymbol.get
    val decls = cls.declaredMethods
    val labels = decls.map(m => ConstantType(StringConstant(m.name)))

    def encodeMeta(annot: Term): Type[?] =
      if annot.tpe <:< TypeRepr.of[MetaAnnotation] then
        AnnotatedType(TypeRepr.of[Meta], annot).asType
      else
        report.error(s"annotation ${annot.show} does not extend ${Type.show[MetaAnnotation]}", annot.pos)
        Type.of[Meta]

    def decodeResult(res: Type[?]): (Type[?], Type[?]) = res match
      case '[Either[e, o]] =>
        (Type.of[e], Type.of[o])
      case '[Right[e, o]] =>
        (Type.of[e], Type.of[o])
      case '[Left[e, o]] =>
        (Type.of[e], Type.of[o])
      case '[tpe] =>
        (Type.of[VoidType], Type.of[tpe])

    val ops = decls.map(method =>
      val meta = typesToTuple(method.annotations.map(encodeMeta))
      val (inputTypes, inputLabels, inputMetas, error, output) =
        tpe.memberType(method) match
          case ByNameType(res) =>
            val (error, output) = decodeResult(res.asType)
            (Nil, Nil, Nil, error, output)
          case MethodType(paramNames, paramTpes, res) =>
            val inputTypes = paramTpes.map(_.asType)
            val inputLabels = paramNames.map(l => ConstantType(StringConstant(l)).asType)
            val inputMetas = method.paramSymss.head.map(s => typesToTuple(s.annotations.map(encodeMeta)))
            val (error, output) = res match
              case _: MethodType => report.errorAndAbort(s"curried method ${method.name} is not supported")
              case _: PolyType => report.errorAndAbort(s"curried method ${method.name} is not supported")
              case _ => decodeResult(res.asType)
            (inputTypes, inputLabels, inputMetas, error, output)
          case _: PolyType => report.errorAndAbort(s"generic method ${method.name} is not supported")
      val inTup = typesToTuple(inputTypes)
      val inLab = typesToTuple(inputLabels)
      val inMet = typesToTuple(inputMetas)
      (meta, inTup, inLab, inMet, error, output) match
        case ('[m], '[i], '[l], '[iM], '[e], '[o]) => Type.of[Operation {
          type Metadata = m
          type InputTypes = i
          type InputLabels = l
          type InputMetadatas = iM
          type ErrorType = e
          type OutputType = o
        }]

    )
    val opsTup = typesToTuple(ops.toList)
    val labelsTup = typesToTuple(labels.map(_.asType))
    val name = ConstantType(StringConstant(cls.name)).asType
    (opsTup, labelsTup, name) match
      case ('[ops], '[labels], '[label]) => '{ (new OpsMirror {
        type MirroredType = T
        type MirroredLabel = label
        type MirroredOperations = ops
        type MirroredOperationLabels = labels
      }): OpsMirror.Of[T] {
        type MirroredLabel = label
        type MirroredOperations = ops
        type MirroredOperationLabels = labels
      }}
