package es.eriktorr

import board._
import game._
import infrastructure.FakeInMemoryBoard
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
          marksRef <- Ref.of[IO, List[Mark]](List.empty)
          board = FakeInMemoryBoard(marksRef)
          game = Game.start[IO](board)
          mark = Mark(player, position)
          _ <- game.next(mark)
          outcome <- game.solve
          finalMarks <- marksRef.get
        } yield expect(outcome.isEmpty) && expect(finalMarks == List(mark))
    }
  }

  simpleTest("players alternate to play") {
    expect(false)
  }

  simpleTest("there can only be one mark for each position on the board") {
    expect(false)
  }
}
