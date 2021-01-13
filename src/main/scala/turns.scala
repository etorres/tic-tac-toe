package es.eriktorr

import player._

object turns {
  trait Turns[F[_]] {
    def advance(player: Player): F[Unit]
  }
}
