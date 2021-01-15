package es.eriktorr

import board._
import player._

import cats._
import cats.effect._
import cats.implicits._

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
      override def next(mark: Mark): F[Unit] = board.add(mark)

      override def solve: F[Option[Outcome]] = {
        val F = Monad[F]
        F.ifM(board.marksLeft.map(_ == 0))(
          ifTrue = F.pure(Draw.some),
          ifFalse = board.playerWithThreeMarksInARow
            .map {
              case Some(player) => Winner(player).some
              case None => none[Outcome]
            }
        )
      }
    }
  }
}
