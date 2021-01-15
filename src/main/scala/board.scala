package es.eriktorr

import player._

import cats._
import cats.data._
import cats.derived._

object board {
  sealed trait Position extends Product with Serializable

  case object TopLeftCorner extends Position
  case object TopCenter extends Position
  case object TopRightCorner extends Position
  case object CenterLeft extends Position
  case object Center extends Position
  case object CenterRight extends Position
  case object BottomLeftCorner extends Position
  case object BottomCenter extends Position
  case object BottomRightCorner extends Position

  implicit val eqPosition: Eq[Position] = semiauto.eq

  val allPositions: NonEmptyList[Position] = NonEmptyList.of(
    TopLeftCorner,
    TopCenter,
    TopRightCorner,
    CenterLeft,
    Center,
    CenterRight,
    BottomLeftCorner,
    BottomCenter,
    BottomRightCorner
  )

  val diagonals: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeftCorner, Center, BottomRightCorner),
      Set(TopRightCorner, Center, BottomLeftCorner)
    )

  val horizontalRows: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeftCorner, TopCenter, TopRightCorner),
      Set(CenterLeft, Center, CenterRight),
      Set(BottomLeftCorner, BottomCenter, BottomRightCorner)
    )

  val verticalRows: NonEmptyList[Set[Position]] =
    NonEmptyList.of(
      Set(TopLeftCorner, CenterLeft, BottomLeftCorner),
      Set(TopCenter, Center, BottomCenter),
      Set(TopRightCorner, CenterRight, BottomRightCorner)
    )

  final case class Mark(player: Player, position: Position)

  implicit val showMark: Show[Mark] = semiauto.show

  trait Board[F[_]] {
    def add(mark: Mark): F[Unit]

    def playerWithThreeMarksInARow: F[Option[Player]]

    def marksLeft: F[Int]
  }
}
