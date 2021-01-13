package es.eriktorr

import board._
import effect._
import error._
import infrastructure.GameContext.GameState.newGame
import infrastructure.GameContext.withGameContext
import infrastructure.Generators._
import player._

import cats._
import cats.data._
import cats.derived._
import cats.implicits._
import weaver._
import weaver.scalacheck._

object PlayGameSuite extends SimpleIOSuite with IOCheckers {
  final case class TestCase(marks: NonEmptyList[Mark])

  object TestCase {
    implicit val showTestCase: Show[TestCase] = semiauto.show
  }

  simpleTest("the game starts with the first player making an X mark in the board") {
    val gen = for {
      player <- crossesGen
      position <- positionGen
    } yield TestCase(NonEmptyList.one(Mark(player, position)))

    forall(gen) {
      case TestCase(marks) =>
        withGameContext(newGame) { game =>
          game.next(marks.head) *> game.solve
        } map {
          case (finalState, outcome) =>
            expect.all(finalState.marks == marks.toList, outcome.isEmpty)
        }
    }
  }

  simpleTest("starting with a 0 is not possible") {
    failure("feature under development")
  }

  simpleTest("players alternate to play") {
    implicit val playerOrder: Order[Player] = Order.fromLessThan((a, _) => a.eq(Crosses))

    val gen = for {
      players <- nDistinct(2, playerGen)
      positions <- nDistinct(2, positionGen)
      marks = (players zip positions).map { case (player, position) => Mark(player, position) }
    } yield TestCase(NonEmptyList.fromListUnsafe(marks).sortBy(_.player))

    forall(gen) {
      case TestCase(marks) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next)
        } map {
          case (finalState, outcome) =>
            expect.all(finalState.marks == marks.toList.reverse, outcome.isEmpty)
        }
    }
  }

  simpleTest("players must wait their turn to play") {
    failure("feature under development")
  }

  simpleTest("there can only be one mark for each position on the board") {
    val gen = for {
      player <- crossesGen
      position <- positionGen
    } yield {
      val mark = Mark(player, position)
      TestCase(NonEmptyList.of(mark, mark))
    }

    forall(gen) {
      case TestCase(marks) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next).extractError[InvalidMove]
        } map {
          case (_, error) =>
            expect(error == s"A mark already exist in the board: ${marks.head.show}".some)
        }
    }
  }

  simpleTest(
    "to win the game, a player must place three of their marks in a horizontal, vertical, or diagonal row"
  ) {
    failure("feature under development")
  }
}
