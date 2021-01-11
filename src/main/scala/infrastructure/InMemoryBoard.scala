package es.eriktorr
package infrastructure

import board._
import error._

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

  val addOrFail: (Mark, Ref[IO, List[Mark]]) => IO[Unit] =
    (mark: Mark, ref: Ref[IO, List[Mark]]) =>
      for {
        maybeModified <- ref.modify { currentMarks =>
          currentMarks.find(_.eq(mark)) match {
            case Some(_) =>
              currentMarks -> InvalidMove(
                s"A mark already exist in the board: ${mark.show}"
              ).asLeft
            case None => (mark :: currentMarks) -> ().asRight
          }
        }
        result <- IO.fromEither(maybeModified)
      } yield result
}
