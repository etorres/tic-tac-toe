package es.eriktorr
package infrastructure

import board._
import game._

import cats.effect._
import cats.effect.concurrent.Ref

object GameContext {
  final case class GameState(marks: List[Mark])

  object GameState {
    def newGame: GameState = GameState(marks = List.empty)
  }

  def withGameContext[A](initialState: GameState)(runTest: Game[IO] => IO[A]): IO[(GameState, A)] =
    for {
      marksRef <- Ref.of[IO, List[Mark]](initialState.marks)
      board = FakeInMemoryBoard(marksRef)
      game = Game.start[IO](board)
      testResult <- runTest(game)
      finalMarks <- marksRef.get
    } yield (initialState.copy(marks = finalMarks), testResult)
}
