package mirrorops

import quoted.*

import scala.util.chaining.given

trait OpsMirror:
  type MirroredType
  type MirroredLabel
  type MirroredOperations
  type MirroredOperationLabels

sealed trait Meta

trait Operation:
  type Metadata <: Tuple
  type InputTypes <: Tuple
  type InputLabels <: Tuple
  type InputMetadatas <: Tuple
  type OutputType
  type OutputMetadata <: Tuple

object OpsMirror:
  type Of[T] = OpsMirror { type MirroredType = T }

  transparent inline given reify[T]: Of[T] = ${ reifyImpl[T] }

  case class Metadata(base: List[Expr[Any]], inputs: List[List[Expr[Any]]], output: List[Expr[Any]])

  def metadata[Op: Type](using Quotes): Metadata =
    import quotes.reflect.*

    def extractMetass[Metadatas: Type]: List[List[Expr[Any]]] = Type.of[Metadatas] match
      case '[m *: ms] => extractMetas[m] :: extractMetass[ms]
      case '[EmptyTuple] => Nil

    def extractMetas[Metadata: Type]: List[Expr[Any]] = Type.of[Metadata] match
      case '[m *: ms] => extractMeta[m]  :: extractMetas[ms]
      case '[EmptyTuple] => Nil

    def extractMeta[M: Type]: Expr[Any] = TypeRepr.of[M] match
      case AnnotatedType(_, annot) =>
        annot.asExpr
      case tpe =>
        report.errorAndAbort(s"got the metadata element ${tpe.show}")

    Type.of[Op] match
      case '[Operation {
        type Metadata = metadata
        type InputMetadatas = inputMetadatas
        type OutputMetadata = outputMetadata
      }] => Metadata(extractMetas[metadata], extractMetass[inputMetadatas], extractMetas[outputMetadata])
      case _ => report.errorAndAbort("expected an Operation with Metadata.")

  private def reifyImpl[T: Type](using Quotes): Expr[Of[T]] =
    import quotes.reflect.*

    def toTuple(list: List[Type[?]]): Type[?] = list match
      case '[t] :: ts => toTuple(ts) match
        case '[type ts <: Tuple; ts] => Type.of[t *: ts]
      case _ => Type.of[EmptyTuple]

    val tpe = TypeRepr.of[T]
    val cls = tpe.classSymbol.get
    val decls = cls.declaredMethods
    val labels = decls.map(m => ConstantType(StringConstant(m.name)))

    def encodeMeta(annot: Term): Type[?] =
      AnnotatedType(TypeRepr.of[Meta], annot).asType

    def collectAnnotations(tpe: TypeRepr, acc: List[Type[?]] = Nil): (Type[?], List[Type[?]]) = tpe match
      case AnnotatedType(tpe, annot) => collectAnnotations(tpe, encodeMeta(annot) :: acc)
      case tpe => (tpe.asType, acc)

    val ops = decls.map(method =>
      val meta = toTuple(method.annotations.map(encodeMeta))
      val (inputTypes, inputMetas) =
        tpe.memberType(method).asInstanceOf[MethodType].paramTypes.map(collectAnnotations(_)).unzip
      val inputLabels = tpe.memberType(method).asInstanceOf[MethodType].paramNames
        .map(l => ConstantType(StringConstant(l)).asType)
      val (output, outputMeta) =
        tpe.memberType(method).asInstanceOf[MethodType].resType.pipe(collectAnnotations(_))
      val inTup = toTuple(inputTypes)
      val inLab = toTuple(inputLabels)
      val inMet = toTuple(inputMetas.map(toTuple))
      val outMet = toTuple(outputMeta)
      (meta, inTup, inLab, inMet, output, outMet) match
        case ('[m], '[i], '[l], '[iM], '[o], '[oM]) => Type.of[Operation {
          type Metadata = m
          type InputTypes = i
          type InputLabels = l
          type InputMetadatas = iM
          type OutputType = o
          type OutputMetadata = oM
        }]

    )
    val opsTup = toTuple(ops.toList)
    val labelsTup = toTuple(labels.map(_.asType))
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
