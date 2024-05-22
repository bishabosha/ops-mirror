package serverlib.ziohttp

import serverlib.HttpService.Empty
import serverlib.HttpService.Endpoints.Endpoint

import zio.IO
import zio.UIO
import zio.ZIO

class Server

object Server:
  class Handler[I <: Tuple, E, O]

  type Func[I <: Tuple, E, O] = I match
    case EmptyTuple => () => Res[E, O]
    case a *: EmptyTuple => a => Res[E, O]
    case (a, b) => (a, b) => Res[E, O]

  type Res[E, O] = E match
    case Empty => UIO[O]
    case _ => IO[E, O]

  extension [N, I <: Tuple, E, O](e: Endpoint[N, I, E, O])
    def handle(op: Func[I, E, O]): Handler[I, E, O] =
      Handler[I, E, O]()

  class ServerBuilder:

    def addEndpoint[I <: Tuple, E, O](handler: Handler[I, E, O]): ServerBuilder =
      new ServerBuilder // simulate adding an endpoint

    def create(port: Int) = ZIO.acquireRelease(ZIO.succeed(new Server))(_ => ZIO.unit)
