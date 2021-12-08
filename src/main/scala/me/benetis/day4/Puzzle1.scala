package me.benetis.day4

import zio.Console._
import zio._

import scala.io.{BufferedSource, Source}

object Puzzle1 extends ZIOAppDefault {

  case class Draw(value: Int)
  case class BingoLine(value: Vector[Int])
  case class BingoGrid(lines: Vector[BingoLine])

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    _ <- printLine("Begin..")
    lines <- ZIO.acquireReleaseWith(
      Task(Source.fromResource("input_day4.txt")),
      (buff: BufferedSource) => UIO(buff.close()),
      (buff: BufferedSource) => Task(buff.getLines().toVector)
    )

    numbersToDraw <- readNumbersToDraw(lines.head)
    grids <- readBingoGrids(lines.tail)
    _ <- printLine(grids)
  } yield ()

  private def readNumbersToDraw(line: String): Task[Vector[Draw]] = Task {
    line.split(",").filter(_.nonEmpty).map(_.toInt).map(Draw).toVector
  }

  private def readBingoGrids(
      allBingoLines: Vector[String]
  ): Task[Vector[BingoGrid]] = {
    ZIO.collectAll(allBingoLines.grouped(6).map(readBingoGrid).toVector)
  }

  private def readBingoLine(line: String): Task[BingoLine] = Task {
    BingoLine(line.split(" ").filter(_.nonEmpty).map(_.toInt).toVector)
  }

  private def readBingoGrid(
      oneBingoGrid: Vector[String]
  ): Task[BingoGrid] = {
    ZIO
      .foreach(oneBingoGrid)(readBingoLine)
      .map(_.filter(_.value.nonEmpty))
      .map(BingoGrid)
  }

}
