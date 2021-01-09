package es.eriktorr

import board._
import game._
import infrastructure.{BoardState, FakeBoard}
import player._

import cats._
import cats.derived._
import cats.effect._
import cats.effect.concurrent.Ref
import org.scalacheck._
import weaver._
import weaver.scalacheck._

object PlayGameSuite extends SimpleIOSuite with IOCheckers {
  def positionGen: Gen[Position] = Gen.oneOf(allPositions)

  simpleTest("the game starts with the first player making an X mark in the board") {
    final case class TestCase(player: Player, position: Position)

    object TestCase {
      implicit val showTestCase: Show[TestCase] = semiauto.show
    }

    val gen = for {
      player <- Gen.const(Crosses)
      position <- positionGen
    } yield TestCase(player, position)

    forall(gen) {
      case TestCase(player, position) =>
        for {
          boardState <- Ref.of(BoardState(List.empty))
          board = FakeBoard(boardState)
          game = Game.start[IO](board)
          mark = Mark(player, position)
          _ <- game.next(mark)
          outcome <- game.solve
          finalBoardState <- boardState.get
        } yield expect(outcome.isEmpty) && expect(finalBoardState.marks == List(mark))
    }
  }
}
