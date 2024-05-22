package app

import language.experimental.namedTuples

import serverlib.*

import HttpService.model.*, source.*, method.*

import scala.collection.concurrent.TrieMap
import syntax.*
import mirrorops.OpsMirror

@failsWith[Int]
trait GreetService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit


val e = HttpService.endpoints[GreetService]

@main def server =
  import jdkhttp.Server, Server.*
  // import ziohttp.Server.*

  val greetings = TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoints(e):
      (
        e.greet.handle(name => Right(s"${greetings.getOrElse(name, "Hello")}, $name")),
        e.setGreeting.handle((name, greeting) => Right(greetings(name) = greeting)),
      )
    .create(port = 8081)

  sys.addShutdownHook(server.close())

@main def client(who: String, newGreeting: String) =
  import jdkhttp.PartialRequest

  val baseURL = "http://localhost:8081"

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
