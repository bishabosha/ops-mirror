package serverlib

import quoted.*
import mirrorops.OpsMirror

case class get(route: String) extends scala.annotation.RefiningAnnotation

sealed trait Source

case class path() extends scala.annotation.RefiningAnnotation with Source

trait Model[T]:
  val services: List[Service]

case class Input(label: String, tpe: Tag[?], source: Source)

case class Service(route: get, inputs: Seq[Input], output: Tag[?])

enum Tag[T]:
  case String extends Tag[String]

object Model:

  inline def derived[T](using m: OpsMirror.Of[T]): Model[T] = ${ Macros.derivedImpl[T]('m) }
