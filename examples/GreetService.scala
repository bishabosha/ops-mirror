package app

import serverlib.*

import HttpService.model.*, source.*, method.*

import scala.collection.concurrent.TrieMap
import syntax.*
import mirrorops.OpsMirror

@fail[Int]
trait GreetService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit


@main def server =
  import jdkhttp.Server.*
  // import ziohttp.Server.*

  val e = HttpService.endpoints[GreetService]

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

  val e = HttpService.endpoints[GreetService]
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


import scala.util.boundary, boundary.{Label, break}

object syntax:
  def either[A, B](op: Label[Left[A, Nothing]] ?=> B): Either[A, B] =
    boundary[Either[A, B]]:
      Right(op)

  extension [A, B](e: Either[A, B]) def ?(using l: Label[Left[A, Nothing]]): B = e match
    case Right(b) => b
    case Left(a) => break(Left(a))
