package es.eriktorr
package infrastructure

import board._

import cats.effect._
import cats.effect.concurrent.Ref

final class FakeInMemoryBoard private (val ref: Ref[IO, List[Mark]]) extends InMemoryBoard {
  override def add(mark: Mark): IO[Unit] = maybeAdd(mark, x => addOrFail(x, ref))
}

object FakeInMemoryBoard {
  def apply(ref: Ref[IO, List[Mark]]): FakeInMemoryBoard = new FakeInMemoryBoard(ref)
}
