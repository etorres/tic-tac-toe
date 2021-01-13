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

  simpleTest("players alternate taking turns to play") {
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
    val gen = for {
      player <- noughtsGen
      position <- positionGen
    } yield TestCase(NonEmptyList.one(Mark(player, position)))

    forall(gen) {
      case TestCase(marks) =>
        withGameContext(newGame) { game =>
          game.next(marks.head).extractError[InvalidMove]
        } map {
          case (_, errors) =>
            expect(errors == List(s"The game cannot start with a ${Noughts.toString}"))
        }
    }
  }

  simpleTest("there can only be one mark for each position on the board") {
    val gen = for {
      players <- nDistinct(2, playerGen)
      position <- positionGen
      positions = List.fill(2)(position)
      marks = (players zip positions).map { case (player, position) => Mark(player, position) }
    } yield TestCase(NonEmptyList.fromListUnsafe(marks).sortBy(_.player))

    forall(gen) {
      case TestCase(marks) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next).extractError[InvalidMove]
        } map {
          case (_, errors) =>
            expect(
              errors == List(
                s"A mark already exist in the position: ${marks.reverse.head.position.toString}"
              )
            )
        }
    }
  }

  simpleTest(
    "to win the game, a player must place three of their marks in a horizontal, vertical, or diagonal row"
  ) {
    failure("feature under development")
  }
}
