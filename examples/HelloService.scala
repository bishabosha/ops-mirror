package app

import serverlib.*

trait HelloService derives Model:
  @get("/hello/{name}")
  def hello(@path name: String): String

@main def run =
  val mm = mirrorops.OpsMirror.reify[app.HelloService]

  val m = summon[Model[HelloService]] // TODO: make 'selectable' so you can bind handlers
  m.services.foreach(println(_))
