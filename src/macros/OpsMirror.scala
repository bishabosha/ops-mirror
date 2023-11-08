import quoted.*
import scala.collection.immutable.Stream.Cons

trait OpsMirror:
  type MirroredType
  type MirroredLabel
  type MirroredOperationLabels

trait Operation:
  type Metadata <: Tuple
  type InputTypes <: Tuple
  type InputLabels <: Tuple
  type Output

object OpsMirror:
  type Of[T] = OpsMirror { type MirroredType = T }

  transparent inline given reify[T]: Of[T] = ${ reifyImpl[T] }

  def reifyImpl[T: Type](using Quotes): Expr[Of[T]] =
    import quotes.reflect.*

    def toTuple(list: List[Type[?]]): Type[?] = list match
      case '[t] :: ts => toTuple(ts) match
        case '[ts] => AppliedType(TypeRepr.of[*:], List(TypeRepr.of[t], TypeRepr.of[ts])).asType

      case _ => Type.of[EmptyTuple]

    val tpe = TypeRepr.of[T]
    val cls = tpe.classSymbol.get
    val decls = cls.declaredMethods
    val labels = decls.map(m => ConstantType(StringConstant(m.name)))
    val ops = decls.map(method =>
      val meta = method.annotations.foldLeft(TypeRepr.of[Any]) { (acc, annot) =>
        AnnotatedType(acc, annot)
      }.asType
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
