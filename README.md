# ops-mirror

Answering the question of "what if my fancy endpoints were defined as a trait"

## Motivating example

As an alternative to endpoint libraries, e.g. Tapir, endpoints4s, zio-http, how about a plain trait + annotations?

```scala
@fail[Int]
trait HelloService derives HttpService:
  @get("/greet/{name}")
  def greet(@path name: String): String

  @post("/greet/{name}")
  def setGreeting(@path name: String, @body greeting: String): Unit
```

then derive servers/clients as such:

```scala
@main def server =
  val e = Endpoints.of[HelloService]

  val greetings = concurrent.TrieMap.empty[String, String]

  val server = ServerBuilder()
    .addEndpoint:
      e.greet.handle(name => Right(s"${greetings.getOrElse(name, "Hello")}, $name"))
    .addEndpoint:
      e.setGreeting.handle((name, greeting) => Right(greetings(name) = greeting))
    .create(port = 8080)
```

```scala
@main def client(name: String, newGreeting: String) =
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
```
