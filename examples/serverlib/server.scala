package serverlib

import quoted.*
import mirrorops.{OpsMirror, MetaAnnotation}


enum method extends MetaAnnotation:
  case get(route: String)
  case post(route: String)
  case put(route: String)

enum source extends MetaAnnotation:
  case path()
  case query()
  case body()

trait Model[T]:
  val services: Map[String, Service[?, ?]]

final class Endpoints[T](val model: Model[T]) extends Selectable:
  def selectDynamic(name: String): Service[?, ?] = model.services(name)

case class Input(label: String, tpe: Tag[?], source: source)

case class Service[I, O](route: method, inputs: Seq[Input], output: Tag[?])

enum Tag[T]:
  case String extends Tag[String]
  case Unit extends Tag[Unit]
  case Int extends Tag[Int]


object Endpoints:
  import scala.util.TupledFunction

  opaque type Endpoint[I, O] <: Service[I, O] = Service[I, O]

  extension [I <: Tuple, O, F](e: Endpoint[I, O])(using tf: TupledFunction[F, I => O])
    def handle(func: F): Unit = ()

  transparent inline def of[T](using m: Model[T], om: OpsMirror.Of[T]): Endpoints[T] =
    ${ ServerMacros.decorateImpl[T]('m, 'om) }


object Model:

  inline def derived[T](using m: OpsMirror.Of[T]): Model[T] = ${ ServerMacros.derivedImpl[T]('m) }
