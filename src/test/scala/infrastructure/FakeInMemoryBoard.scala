package es.eriktorr
package infrastructure

import board._

import cats.effect._
import cats.effect.concurrent.Ref

final class FakeInMemoryBoard private[infrastructure] (val ref: Ref[IO, List[Mark]])
    extends InMemoryBoard {
  override def add(mark: Mark): IO[Unit] = maybeAdd(mark, x => addOrFail(x, ref))

  override def threeInARow: IO[Option[player.Player]] = findThreeInARow(ref)
}

object FakeInMemoryBoard {
  def apply(ref: Ref[IO, List[Mark]]): FakeInMemoryBoard = new FakeInMemoryBoard(ref)
}
