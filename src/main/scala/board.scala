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

  val diagonals: NonEmptyList[NonEmptyList[Position]] =
    NonEmptyList.of(
      NonEmptyList.of(TopLeftCorner, Center, BottomRightCorner),
      NonEmptyList.of(TopRightCorner, Center, BottomLeftCorner)
    )

  val horizontalRows: NonEmptyList[NonEmptyList[Position]] =
    NonEmptyList.of(
      NonEmptyList.of(TopLeftCorner, TopCenter, TopRightCorner),
      NonEmptyList.of(CenterLeft, Center, CenterRight),
      NonEmptyList.of(BottomLeftCorner, BottomCenter, BottomRightCorner)
    )

  val verticalRows: NonEmptyList[NonEmptyList[Position]] =
    NonEmptyList.of(
      NonEmptyList.of(TopLeftCorner, CenterLeft, BottomLeftCorner),
      NonEmptyList.of(TopCenter, Center, BottomCenter),
      NonEmptyList.of(TopRightCorner, CenterRight, BottomRightCorner)
    )

  final case class Mark(player: Player, position: Position)

  implicit val showMark: Show[Mark] = semiauto.show

  trait Board[F[_]] {
    def add(mark: Mark): F[Unit]
  }
}
