package es.eriktorr

import board._
import effect._
import error._
import game.{Winner, Outcome => GameOutcome}
import infrastructure.GameContext.GameState.newGame
import infrastructure.GameContext.withGameContext
import infrastructure.Generators._
import player._

import cats._
import cats.data._
import cats.derived._
import cats.implicits._
import org.scalacheck._
import weaver._
import weaver.scalacheck._

object PlayGameSuite extends SimpleIOSuite with IOCheckers {
  simpleTest("the game starts with the first player making an X mark in the board") {
    val gen = for {
      player <- crossesGen
      position <- positionGen
    } yield TestCase(NonEmptyList.one(Mark(player, position)), none[GameOutcome], none[Player])

    forall(gen) {
      case TestCase(marks, expectedOutcome, _) =>
        withGameContext(newGame) { game =>
          game.next(marks.head) *> game.solve
        } map {
          case (finalState, outcome) =>
            expect.all(finalState.marks == marks.toList, outcome == expectedOutcome)
        }
    }
  }

  simpleTest("the game cannot start with a 0") {
    val gen = for {
      player <- noughtsGen
      position <- positionGen
    } yield TestCase(NonEmptyList.one(Mark(player, position)), none[GameOutcome], player.some)

    forall(gen) {
      case TestCase(marks, _, player) =>
        withGameContext(newGame) { game =>
          game.next(marks.head).extractErrors[InvalidMove]
        } map {
          case (_, errors) =>
            expect(
              errors == List(s"The game cannot start with ${player.stringOrEmpty}")
            )
        }
    }
  }

  simpleTest("players alternate taking turns to play") {
    val gen = for {
      players <- nDistinct(2, playerGen)
      positions <- nDistinct(2, positionGen)
      marks = asMarks(players, positions)
    } yield TestCase(
      NonEmptyList.fromListUnsafe(marks).sortBy(_.player),
      none[GameOutcome],
      none[Player]
    )

    forall(gen) {
      case TestCase(marks, _, _) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next)
        } map {
          case (finalState, _) =>
            expect(finalState.marks == marks.toList.reverse)
        }
    }
  }

  simpleTest("players must wait their turn to play") {
    val gen = for {
      distinctPlayers <- nDistinct(2, playerGen)
      duplicatePlayer <- playerGen
      players = (duplicatePlayer :: distinctPlayers).sorted
      positions <- nDistinct(3, positionGen)
      marks = asMarks(players, positions)
    } yield TestCase(
      NonEmptyList.fromListUnsafe(marks).sortBy(_.player),
      none[GameOutcome],
      duplicatePlayer.some
    )

    forall(gen) {
      case TestCase(marks, _, duplicatePlayer) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next).extractErrors[InvalidMove]
        } map {
          case (_, errors) =>
            expect(
              errors == List(
                s"Player ${duplicatePlayer.stringOrEmpty} plays twice in a row"
              )
            )
        }
    }
  }

  simpleTest("there can only be one mark for each position on the board") {
    val gen = for {
      players <- nDistinct(2, playerGen)
      position <- positionGen
      positions = List.fill(2)(position)
      marks = asMarks(players, positions)
    } yield TestCase(
      NonEmptyList.fromListUnsafe(marks).sortBy(_.player),
      none[GameOutcome],
      none[Player]
    )

    forall(gen) {
      case TestCase(marks, _, _) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next).extractErrors[InvalidMove]
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
    @SuppressWarnings(Array("org.wartremover.warts.TraversableOps"))
    val gen = for {
      players <- nDistinct(2, playerGen)
      (winner, loser) = (players.head, players.last)
      winnerPositions <- for {
        strategy <- Gen.oneOf(diagonals, horizontalRows, verticalRows)
        positions <- Gen.oneOf(strategy.toList)
      } yield positions.toList
      loserPositions <- nDistinct(
        3,
        Gen.oneOf(allPositions.filterNot(winnerPositions.contains_(_)))
      )
      (winnerMarks, loserMarks) = (
        asMarks(List.fill(3)(winner), winnerPositions),
        asMarks(List.fill(3)(loser), loserPositions)
      )
      allMarks = loserMarks.intersperse(winnerMarks)
    } yield TestCase(
      NonEmptyList
        .fromListUnsafe(if (allMarks.head.player.eq(Crosses)) allMarks else allMarks.tail),
      Winner(winner).some,
      none[Player]
    )

    forall(gen) {
      case TestCase(marks, expectedOutcome, _) =>
        withGameContext(newGame) { game =>
          marks.foldMap(game.next) *> game.solve
        } map {
          case (finalState, outcome) =>
            expect.all(finalState.marks == marks.toList.reverse, outcome == expectedOutcome)
        }
    }
  }

  simpleTest("there is a draw when players cannot make any move") {
    failure("feature under development")
  }

  final case class TestCase(
    marks: NonEmptyList[Mark],
    expectedOutcome: Option[GameOutcome],
    player: Option[Player]
  )

  implicit val showTestCase: Show[TestCase] = semiauto.show

  private[this] def asMarks(players: List[Player], positions: List[Position]): List[Mark] =
    (players zip positions).map {
      case (player, position) => Mark(player, position)
    }
}
