import quoted.*
case class get(route: String) extends scala.annotation.Annotation

trait Model[T]:
  val services: List[Service]

case class Service(route: get)

object Model:
  inline def derived[T]: Model[T] = ${ derivedImpl[T] }

  def derivedImpl[T: Type](using Quotes): Expr[Model[T]] =
    import quotes.reflect.*

    val mirror: Expr[OpsMirror.Of[T]] = Expr.summon[OpsMirror.Of[T]].get

    def extractServices[Ts: Type]: List[Expr[Service]] = Type.of[Ts] match
      case '[op *: ts] => extractService[op] :: extractServices[ts]
      case '[EmptyTuple] => Nil

    def extractService[O: Type]: Expr[Service] = Type.of[O] match
      case '[Operation { type Metadata = meta }] =>
        TypeRepr.of[meta] match
          case AnnotatedType(_, annot) =>
            '{ Service(${annot.asExprOf[get]}) }

    val servicesExpr = mirror match
      case '{ $m: OpsMirror.Of[T] { type MirroredOperations } } =>
        report.error("got the mirror" + m.show)
        // Expr.ofList(extractServices[mirroredOps])
    '{
      new Model[T] {
        val services = Nil// $servicesExpr
      }
    }

  // type Mirror = OpsMirror:
  //   type MirroredType = HelloService
  //   type MirroredLabel = "HelloService"
  //   type MirroredOperations = (
  //     Operation {
  //       type Metadata = Any @get("/hello/:name")
  //       type InputTypes = String *: EmptyTuple
  //       type InputLabels = "name" *: EmptyTuple
  //       type Output = String
  //     }
  //   ) *: EmptyTuple
  //   type MirroredOperationLabels = "hello" *: EmptyTuple