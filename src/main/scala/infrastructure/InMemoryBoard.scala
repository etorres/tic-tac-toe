package es.eriktorr
package infrastructure

import board._
import error._
import player._

import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._

/** Create new instances in production with:
 * {{{
 * for {
 *   board <- Ref.of[IO, List[Mark]](List.empty).map(new InMemoryBoard(_))
 *   game = Game(board)
 * } yield game
 * }}}
 */
trait InMemoryBoard extends Board[IO] {
  def maybeAdd(mark: Mark, f: Mark => IO[Unit]): IO[Unit] = f.apply(mark)

  type ValidationResult[A] = ValidatedNec[String, A]

  private[this] def validatePlayer(
    mark: Mark,
    currentMarks: List[Mark]
  ): ValidationResult[Player] = {
    val player = mark.player
    currentMarks.headOption match {
      case Some(lastMark) =>
        if (!player.eq(lastMark.player)) player.validNec
        else s"Player ${player.toString} plays twice in a row".invalidNec
      case None =>
        if (player.eq(Crosses)) player.validNec
        else s"The game cannot start with a ${player.toString}".invalidNec
    }
  }

  private[this] def validatePosition(
    mark: Mark,
    currentMarks: List[Mark]
  ): ValidationResult[Position] = {
    val position = mark.position
    currentMarks.find(_.position.equals(position)) match {
      case None => position.validNec
      case Some(_) => s"A mark already exist in the position: ${position.toString}".invalidNec
    }
  }

  private[this] def validateMove(mark: Mark, currentMarks: List[Mark]): ValidationResult[Mark] =
    (validatePlayer(mark, currentMarks), validatePosition(mark, currentMarks)).mapN(
      (player, position) => Mark(player, position)
    )

  val addOrFail: (Mark, Ref[IO, List[Mark]]) => IO[Unit] =
    (mark: Mark, ref: Ref[IO, List[Mark]]) =>
      for {
        maybeModified <- ref.modify { currentMarks =>
          validateMove(mark, currentMarks) match {
            case Validated.Valid(validMark) => (validMark :: currentMarks) -> ().asRight
            case Validated.Invalid(errors) =>
              currentMarks -> InvalidMove(errors.toNonEmptyList).asLeft
          }
        }
        result <- IO.fromEither(maybeModified)
      } yield result
}
