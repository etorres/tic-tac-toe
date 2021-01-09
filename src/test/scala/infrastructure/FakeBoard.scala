package es.eriktorr
package infrastructure

import board._

import cats.effect._
import cats.effect.concurrent.Ref

final case class BoardState(marks: List[Mark])

final class FakeBoard private (val ref: Ref[IO, BoardState]) extends Board[IO] {}

object FakeBoard {
  def apply(ref: Ref[IO, BoardState]): FakeBoard = new FakeBoard(ref)
}
