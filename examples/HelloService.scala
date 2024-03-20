package app

import serverlib.*

import HttpService.model.*, source.*, method.*

import scala.collection.concurrent.TrieMap


@fail[Int]
trait HelloService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit


@main def server =
  import jdkhttp.Server.*
  // import ziohttp.Server.*

  val e = Endpoints.of[HelloService]

  e.model.routes.foreach((k, r) => println(s"$k: $r"))

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.greet.handle(name => Right(s"${greetings.getOrElse(name, "Hello")}, $name"))
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => Right(greetings(name) = greeting))
    .create(port = 8080)

  sys.addShutdownHook(server.close())

@main def client(who: String, newGreeting: String) =
  import jdkhttp.PartialRequest

  val e = Endpoints.of[HelloService]

  val greetRequest = PartialRequest(e.greet, "http://localhost:8080")
    .prepare(who)

  val setGreetingRequest = PartialRequest(e.setGreeting, "http://localhost:8080")
    .prepare(who, newGreeting)

  val greetRequest2 = PartialRequest(e.greet, "http://localhost:8080")
    .prepare(who)

  for
    init    <- greetRequest.send()
    _       <- setGreetingRequest.send()
    updated <- greetRequest2.send()
  do
    println(s"greeting for $who was: $init, now is: $updated")
