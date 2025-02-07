package serverlib.util

import scala.util.boundary, boundary.{Label, break}

object eitherSyntax:
  inline def either[A, B](inline op: Label[Left[A, Nothing]] ?=> B): Either[A, B] =
    boundary[Either[A, B]]:
      Right(op)

  extension [A, B](e: Either[A, B])
    inline def ?(using inline l: Label[Left[A, Nothing]]): B = e match
      case Right(b) => b
      case Left(a)  => break(Left(a))
end eitherSyntax
