package serverlib

import quoted.*
import mirrorops.{OpsMirror, MetaAnnotation}

trait HttpService[T]:
  val routes: Map[String, HttpService.Route[?, ?, ?]]

object HttpService:
  inline def derived[T](using m: OpsMirror.Of[T]): HttpService[T] = ${ ServerMacros.derivedImpl[T]('m) }

  sealed trait Empty

  object model:
    enum method extends MetaAnnotation:
      case get(route: String)
      case post(route: String)
      case put(route: String)

    enum source extends MetaAnnotation:
      case path()
      case query()
      case body()

  case class Input(label: String, tpe: Tag[?], source: model.source)
  case class Route[I, E, O](route: model.method, inputs: Seq[Input], error: Tag[?], output: Tag[?])

  enum Tag[T]:
    case String extends Tag[String]
    case Unit extends Tag[Unit]
    case Int extends Tag[Int]
    case Empty extends Tag[Empty]
