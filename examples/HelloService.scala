package app

import serverlib.{Model, get}

trait HelloService derives Model:
  @get("/hello/{name}")
  def hello(name: String): String

@main def run =
  val m = summon[Model[HelloService]]
  m.services.foreach(println(_))
