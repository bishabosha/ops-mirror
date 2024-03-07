package app

import serverlib.*

import HttpService.model.*, source.*, method.*
import jdkhttp.Server.*

import scala.collection.concurrent.TrieMap

trait HelloService derives HttpService:
  @get("/hello/{name}")
  def hello(@path name: String): String

  @post("/greeting/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit

  @get("/foo/{id}")
  def foo(@path id: Int): String

  @put("/year")
  def getYear: Int

@main def demo =
  val mm = summon[mirrorops.OpsMirror.Of[HelloService]]

  val e = Endpoints.of[HelloService]

  e.model.routes.foreach((k, r) => println(s"$k: $r"))

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.hello.handle(name => s"${greetings.getOrElse(name, "Hello")}, $name\n")
    .addEndpoint:
      e.getYear.handle(() => 2021)
    .addEndpoint:
      e.foo.handle(id => s"foo $id * 71 = ${id * 71}\n")
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => greetings(name) = greeting)
    .create()

  sys.addShutdownHook(server.close())


