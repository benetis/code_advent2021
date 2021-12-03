package me.benetis.day1

import zio._
import zio.Console._
import scala.util.Try

object Puzzle1 extends ZIOAppDefault {

  case class Depth(value: Int)

  def run(): ZIO[ZEnv with Has[ZIOAppArgs], Throwable, Unit] = for {
    parsed <- parseInput(InputDay1.input)
    count = countHowManyIncreases(parsed)
    _ <- printLine(count)
  } yield ()

  def parseInput(input: String): Task[IndexedSeq[Depth]] = {
    def cleanAndConvert(line: String) = ZIO.fromTry(Try(Depth(line.trim.toInt)))
    ZIO
      .foreach {
        input
          .split("\n")
          .filter(_.nonEmpty)
      }(cleanAndConvert)
      .map(_.toIndexedSeq)
  }

  def countHowManyIncreases(depthChanges: IndexedSeq[Depth]): Int = {
    depthChanges.sliding(2).count(pair => pair(0).value < pair(1).value)
  }

}
