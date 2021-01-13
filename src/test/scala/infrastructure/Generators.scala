package es.eriktorr
package infrastructure

import board._
import player._

import org.scalacheck._

object Generators {
  def nDistinct[T](number: Int, elementGen: Gen[T]): Gen[List[T]] = {
    @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
    def generate(accumulator: List[T]): Gen[List[T]] =
      if (accumulator.size == number) Gen.const(accumulator)
      else
        for {
          candidate <- elementGen
          result <- generate(
            if (accumulator.contains(candidate)) accumulator else candidate :: accumulator
          )
        } yield result

    generate(List.empty)
  }

  def crossesGen: Gen[Player] = Gen.const(Crosses)

  def noughtsGen: Gen[Player] = Gen.const(Noughts)

  def playerGen: Gen[Player] = Gen.oneOf(allPlayers.toList)

  def positionGen: Gen[Position] = Gen.oneOf(allPositions.toList)
}
