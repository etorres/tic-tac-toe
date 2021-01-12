package es.eriktorr
package effect

import error._

import cats.effect._
import cats.implicits._

import scala.reflect.ClassTag

trait IoSyntax {
  implicit class IoOps[A](self: IO[A]) {
    def extractError[E <: Throwable](implicit tag: ClassTag[E]): IO[Option[String]] =
      self.map(_ => none[String]).recoverWith {
        case e: InvalidMove => IO(e.error.some)
        case e: E => IO(e.getMessage.some)
      }
  }
}
