package serverlib

import mirrorops.{OpsMirror, Operation}

import quoted.*

case class get(route: String) extends scala.annotation.RefiningAnnotation

trait Model[T]:
  val services: List[Service]

case class Service(route: get)

object Model:
  inline def derived[T]: Model[T] = ${ derivedImpl[T] }

  private def derivedImpl[T: Type](using Quotes): Expr[Model[T]] =
    import quotes.reflect.*

    val mirror: Expr[OpsMirror.Of[T]] = Expr.summon[OpsMirror.Of[T]].get

    def extractServices[Ts: Type]: List[Expr[Service]] = Type.of[Ts] match
      case '[op *: ts] => extractService[op] :: extractServices[ts]
      case '[EmptyTuple] => Nil

    def extractService[Op: Type]: Expr[Service] =
      val metas = OpsMirror.metadata[Op]
      val route = metas.collectFirst {
        case '{ $g: serverlib.get } => g
      }
      route match
        case Some(r) => '{ Service($r) }
        case None => report.errorAndAbort(s"got the metadata elems ${metas.map(_.show)}")


    val servicesExpr = mirror match
      case '{ $m: OpsMirror.Of[T] { type MirroredOperations = mirroredOps } } =>
        Expr.ofList(extractServices[mirroredOps])

    ('{
      new Model[T] {
        val services = $servicesExpr
      }
    })
  end derivedImpl
end Model
