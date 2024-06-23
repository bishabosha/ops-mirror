# ops-mirror

Answering the question of "what if my fancy endpoints were defined as a trait"

## Usage

Use ops-mirror to help define the `derived` method of a Type-class. It provides a view (`mirrorops.OpsMirror`) over the methods of a trait. This is much more convenient to decompose with quotes/splices, or even match types, than the alternative of using the Reflection API (`scala.quoted.Quotes`).

```scala
//> using dep io.github.bishabosha::ops-mirror::0.1.2

import mirrorops.OpsMirror

// example type-class that defines a Schema of operations.
trait Schema[A] {
  def operations: List[Operation]
}

object Schema {
  // necessary method for `... derives Schema` on a class/trait/enum
  // `(using mirror: OpsMirror.Of[A])` provides a compile-time view on the methods of `A`.
  inline def derived[A](using mirror: OpsMirror.Of[A]): Schema[A] = ???
}
```

## Motivating example

> The following code samples can be found and ran in the [examples](examples) directory.

As an alternative to endpoint libraries, e.g. Tapir, endpoints4s, zio-http, how about a plain trait + annotations?

```scala
@failsWith[Int]
trait GreetService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit
```

`HttpService` is a type-class that stores a collection of HTTP routes. `HttpService.derived` is an inline method that uses `OpsMirror` to reflect on the structure of `GreetService`, converting each method to a route.

Using the HttpService, you can then derive servers/clients as such:

```scala
val e = HttpService.endpoints[GreetService]

@main def server =
  val greetings = concurrent.TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.greet.handle: name =>
        val greeting = greetings.getOrElse(name, "Hello")
        Right(s"$greeting, $name")
    .addEndpoint:
      e.setGreeting.handle: (name, greeting) =>
        greetings(name) = greeting
        Right(())
    .create(port = 8080)

  sys.addShutdownHook(server.close())
end server
    
@main def client(name: String, newGreeting: String) =
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
end client
```

## Publishing

due to the way this project is structured, to publish you need to specify project.scala explicitly and src. This is the only way to ignore examples, but also include the code.

```bash
scala-cli --power publish local project.scala src
```
