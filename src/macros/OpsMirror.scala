package mirrorops

import quoted.*

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
  type Output

object OpsMirror:
  type Of[T] = OpsMirror { type MirroredType = T }

  transparent inline given reify[T]: Of[T] = ${ reifyImpl[T] }

  def metadata[Op: Type](using Quotes): List[Expr[Any]] =
    import quotes.reflect.*

    def extractMetas[Metadata: Type]: List[Expr[Any]] = Type.of[Metadata] match
      case '[m *: ms] => extractMeta[m]  :: extractMetas[ms]
      case '[EmptyTuple] => Nil

    def extractMeta[M: Type]: Expr[Any] = TypeRepr.of[M] match
      case AnnotatedType(_, annot) =>
        annot.asExpr
      case tpe =>
        report.errorAndAbort(s"got the metadata element ${tpe.show}")

    Type.of[Op] match
      case '[Operation { type Metadata = metadata }] => extractMetas[metadata]
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
    val ops = decls.map(method =>
      val meta = toTuple(method.annotations.map(annot =>
        AnnotatedType(TypeRepr.of[Meta], annot).asType
      ))
      val inputTypes = tpe.memberType(method).asInstanceOf[MethodType].paramTypes
      val inputLabels = tpe.memberType(method).asInstanceOf[MethodType].paramNames
        .map(l => ConstantType(StringConstant(l)))
      val output = tpe.memberType(method).asInstanceOf[MethodType].resType.asType
      val inTup = toTuple(inputTypes.map(_.asType))
      val inLab = toTuple(inputLabels.map(_.asType))
      (meta, inTup, inLab, output) match
        case ('[m], '[i], '[l], '[o]) => Type.of[Operation {
          type Metadata = m
          type InputTypes = i
          type InputLabels = l
          type Output = o
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
