package es.eriktorr
package effect

import error._

import cats.effect._
import cats.implicits._

import scala.reflect.ClassTag

trait IoSyntax {
  implicit class IoOps[A](self: IO[A]) {
    def extractErrors[E <: Throwable](implicit tag: ClassTag[E]): IO[List[String]] =
      self.map(_ => List.empty[String]).recoverWith {
        case e: InvalidMove => IO(e.errors.toList)
        case e: E => IO(List(e.getMessage))
      }
  }
}
