package es.eriktorr

import player._

import cats._
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

  val allPositions: List[Position] = List(
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

  final case class Mark(player: Player, position: Position)

  object Mark {
    implicit val showMark: Show[Mark] = semiauto.show
  }

  trait Board[F[_]] {
    def add(mark: Mark): F[Unit]
  }
}
