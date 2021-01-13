package es.eriktorr
package infrastructure

import player._

import cats.effect._
import cats.effect.concurrent.Ref

final class FakeInMemoryTurns private[infrastructure] (ref: Ref[IO, Player]) extends InMemoryTurns {
  override def advance(player: Player): IO[Unit] =
    maybeAdvance(player, x => advanceOrFail(x, ref))
}

object FakeInMemoryTurns {
  def apply(ref: Ref[IO, Player]): FakeInMemoryTurns = new FakeInMemoryTurns(ref)
}
