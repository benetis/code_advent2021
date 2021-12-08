package me.benetis.day4

import zio.Console._
import zio._

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object Puzzle1 extends ZIOAppDefault {

  case class Draw(value: Int)
  case class BingoLineInput(value: Vector[Int])
  case class BingoGridInput(lines: Vector[BingoLineInput])

  case class BingoLineState(value: Vector[Boolean])
  case class BingoGridState(
      lines: Vector[BingoLineState],
      bingoGrid: BingoGridInput
  )

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    _ <- printLine("Begin..")
    lines <- ZIO.acquireReleaseWith(
      Task(Source.fromResource("input_day4.txt")),
      (buff: BufferedSource) => UIO(buff.close()),
      (buff: BufferedSource) => Task(buff.getLines().toVector)
    )
    numbersToDraw <- readNumbersToDraw(lines.head)
    grids <- readBingoGrids(lines.tail)
    initialState = grids.map(grid =>
      BingoGridState(
        grid.lines.map(line => BingoLineState(line.value.map(_ => false))),
        grid
      )
    )
    (winningGrid, lastDraw) = bingo(
      initialState,
      numbersToDraw,
      numbersToDraw.head
    )
    result = findSumOfAllUnmarkedNumbers(winningGrid) * lastDraw.value
    _ <- printLine(result)
  } yield ()

  @tailrec
  def bingo(
      markedState: Vector[BingoGridState],
      toDraw: Vector[Draw],
      lastDraw: Draw
  ): (BingoGridState, Draw) = {
    areWeWinningSon(markedState) match {
      case Some(value) => (value, lastDraw)
      case None =>
        val updated = markedState.map(state => markNumbers(state, toDraw.head))

        bingo(updated, toDraw.tail, toDraw.head)
    }
  }

  private def areWeWinningSon(
      state: Vector[BingoGridState]
  ): Option[BingoGridState] = {
    def horizontalWinner() =
      state.find(_.lines.exists(_.value.forall(_ == true)))
    def verticalWinner() = state.find(gridState =>
      gridState.lines.map(_.value).transpose.exists(_.forall(_ == true))
    )

    horizontalWinner().orElse(verticalWinner())
  }

  private def markNumbers(
      state: BingoGridState,
      draw: Draw
  ): BingoGridState = {

    val updatedLines = state.lines.zipWithIndex.map { case (line, i) =>
      val updatedLineState = {
        line.value.zipWithIndex.map { case (currValue, numIndex) =>
          if (state.bingoGrid.lines(i).value(numIndex) == draw.value)
            true
          else
            currValue
        }
      }

      BingoLineState(updatedLineState)
    }

    state.copy(lines = updatedLines)
  }

  private def findSumOfAllUnmarkedNumbers(gridState: BingoGridState): Int = {
    gridState.lines
      .zip(gridState.bingoGrid.lines)
      .foldLeft(0)((prev, curr) => {
        val (lineState, lineInput) = curr
        prev + sumAllUnmarkedLineMembers(lineState, lineInput)
      })
  }

  private def sumAllUnmarkedLineMembers(
      lineState: BingoLineState,
      lineInput: BingoLineInput
  ): Int = {
    lineState.value.zip(lineInput.value).filterNot(_._1).map(_._2).sum
  }

  private def readNumbersToDraw(line: String): Task[Vector[Draw]] = Task {
    line.split(",").filter(_.nonEmpty).map(_.toInt).map(Draw).toVector
  }

  private def readBingoGrids(
      allBingoLines: Vector[String]
  ): Task[Vector[BingoGridInput]] = {
    ZIO.collectAll(allBingoLines.grouped(6).map(readBingoGrid).toVector)
  }

  private def readBingoLine(line: String): Task[BingoLineInput] = Task {
    BingoLineInput(line.split(" ").filter(_.nonEmpty).map(_.toInt).toVector)
  }

  private def readBingoGrid(
      oneBingoGrid: Vector[String]
  ): Task[BingoGridInput] = {
    ZIO
      .foreach(oneBingoGrid)(readBingoLine)
      .map(_.filter(_.value.nonEmpty))
      .map(BingoGridInput)
  }

}
