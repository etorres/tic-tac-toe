package es.eriktorr

import board._
import player._

import cats.effect._

object game {
  sealed trait Outcome
  final case class Winner(player: Player) extends Outcome
  case object Draw extends Outcome

  trait Game[F[_]] {
    def next(mark: Mark): F[Unit]
    def solve: F[Option[Outcome]]
  }

  object Game {
    def start[F[_]: Sync](board: Board[F]): Game[F] = new Game[F] {
      override def next(mark: Mark): F[Unit] = ???

      override def solve: F[Option[Outcome]] = ???
    }
  }
}