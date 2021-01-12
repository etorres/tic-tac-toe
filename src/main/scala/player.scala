package es.eriktorr

import cats.data._

object player {
  sealed trait Player extends Product with Serializable
  case object Crosses extends Player
  case object Noughts extends Player

  val allPlayers: NonEmptyList[Player] = NonEmptyList.of(Crosses, Noughts)
}
