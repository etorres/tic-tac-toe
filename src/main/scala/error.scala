package es.eriktorr

import scala.util.control.NoStackTrace

object error {
  sealed trait GameError extends NoStackTrace

  final case class InvalidMove(error: String) extends GameError
}
