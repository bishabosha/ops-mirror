package app

import serverlib.*

import HttpService.model.*, source.*, method.*

import scala.collection.concurrent.TrieMap


@fail[Int]
trait HelloService derives HttpService:
  @get("/hello/{name}")
  def hello(@path name: String): String

  @post("/greeting/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit

  @get("/roulette")
  def roulette: String


@main def server =
  import jdkhttp.Server.*
  // import ziohttp.Server.*

  val e = Endpoints.of[HelloService]

  e.model.routes.foreach((k, r) => println(s"$k: $r"))

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.hello.handle(name => Right(s"${greetings.getOrElse(name, "Hello")}, $name"))
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => Right(greetings(name) = greeting))
    .addEndpoint:
      e.roulette.handle(() => Either.cond(scala.util.Random.nextBoolean(), "You win!", -99))
    .create()

  sys.addShutdownHook(server.close())

@main def client(newGreeting: String) =
  import jdkhttp.PartialRequest

  val e = Endpoints.of[HelloService]

  val helloRequest = PartialRequest(e.hello, "http://localhost:8080")
    .prepare("jamie")

  val helloResponse = helloRequest.send()

  println(s"helloResponse: $helloResponse")

  val greetingRequest = PartialRequest(e.setGreeting, "http://localhost:8080")
    .prepare("jamie", newGreeting)

  val _ = greetingRequest.send()

  println("greeting set")

  val helloRequest2 = PartialRequest(e.hello, "http://localhost:8080")
    .prepare("jamie")

  val helloResponse2 = helloRequest2.send()

  println(s"helloResponse2: $helloResponse2")

  val rouletteRequest = PartialRequest(e.roulette, "http://localhost:8080")
    .prepare()

  val rouletteResponse = rouletteRequest.send()

  println(s"rouletteResponse: $rouletteResponse")
