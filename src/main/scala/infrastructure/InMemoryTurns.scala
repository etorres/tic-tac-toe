package es.eriktorr
package infrastructure

import error._
import player._
import turns._

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

/** Create new instances in production with:
 * {{{
 * for {
 *   turns <- Ref.of[IO, Player](Noughts).map(new InMemoryTurns(_))
 *   game = Game(turns)
 * } yield game
 * }}}
 */
trait InMemoryTurns extends Turns[IO] {
  def maybeAdvance(player: Player, f: Player => IO[Unit]): IO[Unit] = f.apply(player)

  val advanceOrFail: (Player, Ref[IO, Player]) => IO[Unit] =
    (player: Player, ref: Ref[IO, Player]) =>
      for {
        maybeModified <- ref.modify { currentPlayer =>
          if (currentPlayer.eq(player))
            currentPlayer -> InvalidMove("Players must wait their turn to play").asLeft
          else player -> ().asRight
        }
        result <- IO.fromEither(maybeModified)
      } yield result
}
