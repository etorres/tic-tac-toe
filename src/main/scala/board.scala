package es.eriktorr

import player._

import cats._
import cats.data._
import cats.derived._

object board {
  sealed trait Position extends Product with Serializable

  case object TopLeft extends Position
  case object TopCenter extends Position
  case object TopRight extends Position
  case object LeftMid extends Position
  case object Center extends Position
  case object RightMid extends Position
  case object BottomLeft extends Position
  case object BottomCenter extends Position
  case object BottomRight extends Position

  implicit val eqPosition: Eq[Position] = semiauto.eq

  val allPositions: NonEmptyList[Position] = NonEmptyList.of(
    TopLeft,
    TopCenter,
    TopRight,
    LeftMid,
    Center,
    RightMid,
    BottomLeft,
    BottomCenter,
    BottomRight
  )

  val diagonals: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeft, Center, BottomRight),
      Set(TopRight, Center, BottomLeft)
    )

  val horizontalRows: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeft, TopCenter, TopRight),
      Set(LeftMid, Center, RightMid),
      Set(BottomLeft, BottomCenter, BottomRight)
    )

  val verticalRows: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeft, LeftMid, BottomLeft),
      Set(TopCenter, Center, BottomCenter),
      Set(TopRight, RightMid, BottomRight)
    )

  final case class Mark(player: Player, position: Position)

  implicit val showMark: Show[Mark] = semiauto.show

  trait Board[F[_]] {
    def add(mark: Mark): F[Unit]

    def playerWithThreeMarksInARow: F[Option[Player]]

    def marksLeft: F[Int]
  }
}
