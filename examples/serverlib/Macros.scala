package serverlib

import scala.quoted.*
import mirrorops.{OpsMirror, Operation}

import scala.util.chaining.given

object Macros:
  def derivedImpl[T: Type](mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[Model[T]] =
    import quotes.reflect.*

    def extractInputs[Ts: Type]: List[Expr[Tag[?]]] = Type.of[Ts] match
      case '[op *: ts] => extractInput[op] :: extractInputs[ts]
      case '[EmptyTuple] => Nil

    def extractInput[T: Type]: Expr[Tag[?]] = Type.of[T] match
      case '[String] => '{ Tag.String }

    def extractServices[Ts: Type]: List[Expr[Service]] = Type.of[Ts] match
      case '[op *: ts] => extractService[op] :: extractServices[ts]
      case '[EmptyTuple] => Nil

    def extractService[Op: Type]: Expr[Service] =
      val metas = OpsMirror.metadata[Op]
      val route = metas.base.collectFirst {
        case '{ $g: serverlib.get } => g
      }
      val ins: Expr[Seq[Input]] = Type.of[Op] match
        case '[type inputLabels <: Tuple; Operation { type InputTypes = inputTypes; type InputLabels = `inputLabels` }] =>
          val labels = Type.valueOfTuple[inputLabels].get.toList.asInstanceOf[List[String]]
          extractInputs[inputTypes].lazyZip(labels).lazyZip(metas.inputs).map((t, l, ms) =>
            val method: Option[Expr[Source]] = ms.collectFirst {
              case '{ $p: serverlib.path } => p
            }
            '{Input(${Expr(l)}, $t, ${method.getOrElse(report.errorAndAbort(s"expected a valid source for param ${l}"))})}
          )
          .pipe(Expr.ofSeq)

      val out = Type.of[Op] match
        case '[Operation { type OutputType = outputType }] =>
          extractInput[outputType]

      route match
        case Some(r) => '{ Service($r, $ins, $out) }
        case None => report.errorAndAbort(s"got the metadata elems ${metas.base.map(_.show)}")


    val servicesExpr = mirror match
      case '{ $m: OpsMirror.Of[T] { type MirroredOperations = mirroredOps } } =>
        Expr.ofList(extractServices[mirroredOps])

    ('{
      new Model[T] {
        val services = $servicesExpr
      }
    })
  end derivedImpl
