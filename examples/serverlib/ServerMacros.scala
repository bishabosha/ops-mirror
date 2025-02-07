package serverlib

import scala.quoted.*
import mirrorops.{OpsMirror, Operation, VoidType}

import scala.util.chaining.given

import HttpService.{Route, Input, model, Empty}
import HttpService.Endpoints, Endpoints.Endpoint

object ServerMacros:

  def derivedImpl[T: Type](mirror: Expr[OpsMirror.Of[T]])(using Quotes): Expr[HttpService[T]] =
    import quotes.reflect.*

    def extractRoutes[Ts: Type]: List[Expr[Route]] = OpsMirror
      .typesFromTuple[Ts]
      .map:
        case '[op] =>
          val metas = OpsMirror.metadata[op]
          val route = metas.base.collectFirst { case '{ $g: model.method } =>
            g
          }
          val ins = Type.of[op] match
            case '[Operation { type InputLabels = inputLabels }] =>
              val labels = OpsMirror.stringsFromTuple[inputLabels]
              val ins =
                labels
                  .lazyZip(metas.inputs)
                  .map((l, ms) =>
                    val method: Option[Expr[model.source]] = ms.collectFirst {
                      case '{ $p: model.source } => p
                    }
                    '{
                      Input(
                        ${ Expr(l) },
                        ${
                          method.getOrElse(
                            report.errorAndAbort(s"expected a valid source for param ${l}")
                          )
                        }
                      )
                    }
                  )
                  .pipe(Expr.ofSeq)
              ins

          route match
            case Some(r) => '{ Route($r, $ins) }
            case None => report.errorAndAbort(s"got the metadata elems ${metas.base.map(_.show)}")
    end extractRoutes

    def extractLabels[T: Type]: List[Expr[String]] =
      val labels = OpsMirror.stringsFromTuple[T]
      labels
        .groupBy(identity)
        .values
        .find(_.sizeIs > 1)
        .foreach: label =>
          report.errorAndAbort(
            s"HttpService does not support overloaded methods, found ${label.head} more than once."
          )
      labels.map(Expr(_))
    end extractLabels

    def extractRoutePairs = mirror match
      case '{
            $m: OpsMirror.Of[T] {
              type MirroredOperations      = mirroredOps
              type MirroredOperationLabels = opLabels
            }
          } =>
        extractLabels[opLabels].zip(extractRoutes[mirroredOps]).map(Expr.ofTuple)
    end extractRoutePairs

    ('{
      new HttpService[T]:
        val routes = scala.collection.immutable.ListMap(${ Varargs(extractRoutePairs) }*)
    })
  end derivedImpl
end ServerMacros
