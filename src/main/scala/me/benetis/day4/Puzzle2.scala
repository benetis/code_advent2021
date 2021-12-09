package me.benetis.day4

import me.benetis.day4.Puzzle1._
import zio.Console._
import zio._

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

//noinspection DuplicatedCode
object Puzzle2 extends ZIOAppDefault {

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
    (losingGrid, lastDraw) = mayTheSquidWin(
      markedState = initialState,
      toDraw = numbersToDraw
    )
    result = findSumOfAllUnmarkedNumbers(losingGrid) * lastDraw.value
    _ <- printLine(result)
  } yield ()

  @tailrec
  def mayTheSquidWin(
      markedState: Vector[BingoGridState],
      toDraw: Vector[Draw]
  ): (BingoGridState, Draw) = {

    val updated =
      markedState.map(state => markNumbers(state, toDraw.head))

    if (updated.size == 1 && isThisBoardWinning(updated.head))
      (updated.head, toDraw.head)
    else {
      val boardsYetToWin = updated.filterNot(isThisBoardWinning)

      mayTheSquidWin(boardsYetToWin, toDraw.tail)
    }
  }

  def isThisBoardWinning(bingoGridState: BingoGridState): Boolean = {
    def horiz() = bingoGridState.lines.exists(_.value.forall(_ == true))
    def vertical() =
      bingoGridState.lines.map(_.value).transpose.exists(_.forall(_ == true))

    horiz() || vertical()
  }
}
