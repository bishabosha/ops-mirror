package app

import serverlib.*

trait HelloService derives Model:
  @get("/hello/{name}")
  def hello(name: String @path): String

@main def run =
  val m = summon[Model[HelloService]] // TODO: make 'selectable' so you can bind handlers
  m.services.foreach(println(_))
