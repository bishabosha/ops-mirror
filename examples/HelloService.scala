package app

import serverlib.*, source.*, method.*

trait HelloService derives Model:
  @get("/hello/{name}")
  def hello(@path name: String): String

  @post("/greeting/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit

  @post("/year")
  def getYear: Int

@main def demo =
  val mm = summon[mirrorops.OpsMirror.Of[HelloService]]

  val e = Endpoints.of[HelloService]

  e.model.services.foreach((k, s) => println(s"$k: $s"))

  e.hello.handle(name => s"Hello, $name")
  e.getYear.handle(() => 2021)
  e.setGreeting.handle((name, greeting) => println(s"$greeting, $name"))


