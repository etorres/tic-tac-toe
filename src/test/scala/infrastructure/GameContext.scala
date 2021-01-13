package es.eriktorr
package infrastructure

import board._
import game._
import player._

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

object GameContext {
  final case class GameState(marks: List[Mark])

  object GameState {
    def newGame: GameState = GameState(marks = List.empty)
  }

  def withGameContext[A](initialState: GameState)(runTest: Game[IO] => IO[A]): IO[(GameState, A)] =
    for {
      (playerRef, marksRef) <- (
        Ref.of[IO, Player](
          Noughts
        ),
        Ref.of[IO, List[Mark]](initialState.marks)
      ).tupled
      (turns, board) = (FakeInMemoryTurns(playerRef), FakeInMemoryBoard(marksRef))
      game = Game.start[IO](turns, board)
      testResult <- runTest(game)
      finalMarks <- marksRef.get
    } yield (initialState.copy(marks = finalMarks), testResult)
}
