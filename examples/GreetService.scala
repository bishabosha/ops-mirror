package app

import language.experimental.namedTuples

import serverlib.*

import HttpService.model.*, source.*, method.*

import scala.collection.concurrent.TrieMap
import mirrorops.OpsMirror
import serverlib.util.eitherSyntax.*

@failsWith[Int]
trait GreetService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit
end GreetService

val e = HttpService.endpoints[GreetService]

@main def server: Unit =
  import jdkhttp.Server.*

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoints(e):
      (
        greet = name => Right(s"${greetings.getOrElse(name, "Hello")}, $name"),
        setGreeting = (name, greeting) => Right(greetings(name) = greeting)
      )
    .create(port = 8080)

  sys.addShutdownHook(server.close())
end server

@main def client(who: String, newGreeting: String): Unit =
  import jdkhttp.PartialRequest

  val baseURL = "http://localhost:8080"

  val greetRequest = PartialRequest(e.greet, baseURL)
    .prepare(who)

  val setGreetingRequest = PartialRequest(e.setGreeting, baseURL)
    .prepare(who, newGreeting)

  either:
    val init = greetRequest.send().?
    setGreetingRequest.send().?
    val updated = greetRequest.send().?
    println(s"greeting for $who was: $init, now is: $updated")
  .fold(sys.exit(_), identity)
end client
