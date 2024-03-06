package serverlib

import quoted.*
import mirrorops.{OpsMirror, MetaAnnotation}

case class get(route: String) extends MetaAnnotation

sealed trait Source

case class path() extends MetaAnnotation with Source

trait Model[T]:
  val services: List[Service]

case class Input(label: String, tpe: Tag[?], source: Source)

case class Service(route: get, inputs: Seq[Input], output: Tag[?])

enum Tag[T]:
  case String extends Tag[String]

object Model:

  inline def derived[T](using m: OpsMirror.Of[T]): Model[T] = ${ Macros.derivedImpl[T]('m) }
