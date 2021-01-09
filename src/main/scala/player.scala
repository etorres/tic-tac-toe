package es.eriktorr

object player {
  sealed trait Player
  case object Crosses extends Player
  case object Noughts extends Player
}
