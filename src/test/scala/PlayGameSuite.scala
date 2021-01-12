package es.eriktorr

import board._
import effect._
import error._
import game._
import infrastructure.FakeInMemoryBoard
import player._

import cats._
import cats.data._
import cats.derived._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import org.scalacheck._
import weaver._
import weaver.scalacheck._

object PlayGameSuite extends SimpleIOSuite with IOCheckers {
  def positionGen: Gen[Position] = Gen.oneOf(allPositions)

  final case class TestCase(marks: NonEmptyList[Mark])

  object TestCase {
    implicit val showTestCase: Show[TestCase] = semiauto.show
  }

  simpleTest("the game starts with the first player making an X mark in the board") {
    val gen = for {
      player <- Gen.const(Crosses)
      position <- positionGen
    } yield TestCase(NonEmptyList.one(Mark(player, position)))

    forall(gen) {
      case TestCase(marks) =>
        for {
          marksRef <- Ref.of[IO, List[Mark]](List.empty)
          board = FakeInMemoryBoard(marksRef)
          game = Game.start[IO](board)
          mark = marks.head
          _ <- game.next(mark)
          outcome <- game.solve
          finalMarks <- marksRef.get
        } yield expect.all(outcome.isEmpty, finalMarks == List(mark))
    }
  }

//  simpleTest("players alternate to play") {
//    failure("feature under development")
//  }

  simpleTest("there can only be one mark for each position on the board") {
    val gen = for {
      player <- Gen.const(Crosses)
      position <- positionGen
    } yield {
      val mark = Mark(player, position)
      TestCase(NonEmptyList.of(mark, mark))
    }

    forall(gen) {
      case TestCase(marks) =>
        for {
          marksRef <- Ref.of[IO, List[Mark]](List.empty)
          board = FakeInMemoryBoard(marksRef)
          game = Game.start[IO](board)
          error <- marks.foldMap(game.next).extractError[InvalidMove]
        } yield expect(error == s"A mark already exist in the board: ${marks.head.show}".some)
    }
  }
}
