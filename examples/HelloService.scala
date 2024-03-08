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

  @get("/roulette")
  @fail[Int]
  def roulette: String


@main def demo =
  val e = Endpoints.of[HelloService]

  e.model.routes.foreach((k, r) => println(s"$k: $r"))

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.hello.handle(name => s"${greetings.getOrElse(name, "Hello")}, $name\n")
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => greetings(name) = greeting)
    .addEndpoint:
      e.roulette.handle(() => Either.cond(scala.util.Random.nextBoolean(), "You win!", -99))
    .create()

  sys.addShutdownHook(server.close())


