package serverlib.util

import scala.util.boundary, boundary.Label, boundary.break

object optional:
  extension [T](t: Option[T])
    inline def ?(using Label[Option[T]]): T =
      t match
        case Some(value) => value
        case None        => break(None)
  end extension

  inline def abort[T](using Label[Option[T]]): Nothing =
    break(None)

  inline def apply[T](op: Label[Option[T]] ?=> T): Option[T] =
    boundary:
      Some(op)
end optional
