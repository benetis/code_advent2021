package me.benetis.day1

import me.benetis.day1.Puzzle1.{Depth, countHowManyIncreases, parseInput}
import zio._
import zio.Console._

object Puzzle2 extends ZIOAppDefault {
  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    parsed <- parseInput(InputDay1.input2)
    count = sliding3CountIncreases(parsed)
    _ <- printLine(count)
  } yield ()

  private def sliding3CountIncreases(depthChanges: IndexedSeq[Depth]): Int = {
    def sumTuple(tuple3: IndexedSeq[Depth]): Depth =
      Depth(tuple3(0).value + tuple3(1).value + tuple3(2).value)

    val summedChanges = depthChanges.sliding(3).map(sumTuple).toIndexedSeq
    Puzzle1.countHowManyIncreases(summedChanges)
  }

}
