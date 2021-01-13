package es.eriktorr

import cats.data._

import scala.util.control.NoStackTrace

object error {
  sealed trait GameError extends NoStackTrace

  final case class InvalidMove(errors: NonEmptyList[String]) extends GameError
}
