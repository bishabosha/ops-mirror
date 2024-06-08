# ops-mirror

Answering the question of "what if my fancy endpoints were defined as a trait"

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

then derive servers/clients as such:

```scala
val e = HttpService.endpoints[GreetService]

@main def server =
  val greetings = concurrent.TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.greet.handle(name => Right(s"${greetings.getOrElse(name, "Hello")}, $name"))
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => Right(greetings(name) = greeting))
    .create(port = 8080)

  sys.addShutdownHook(server.close())
end server
    
@main def client(name: String, newGreeting: String) =
  
  val greetRequest = PartialRequest(e.greet, "http://localhost:8080")
    .prepare(who)

  val setGreetingRequest = PartialRequest(e.setGreeting, "http://localhost:8080")
    .prepare(who, newGreeting)

  either:
    val init = greetRequest.send().?
    setGreetingRequest.send().?
    val updated = greetRequest.send().?
    println(s"greeting for $who was: $init, now is: $updated")
end client
```
