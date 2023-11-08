trait HelloService:
  @get("/hello/:name")
  def hello(name: String): String

object HelloService:
  val g = Model.derived[HelloService]
