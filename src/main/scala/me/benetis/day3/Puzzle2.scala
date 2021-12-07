package me.benetis.day3

import me.benetis.day3.Puzzle1.{BinaryNumberAsSeq, DiagnosticReportLine}
import zio.Console._
import zio._

import scala.annotation.tailrec

object Puzzle2 extends ZIOAppDefault {

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    _ <- printLine("Begin..")
    diagnostics = Puzzle1.parseInput(InputDay3.input)
    oxygen = findLifeSupportRating(
      diagnostics,
      indexToCheck = 0,
      filterEqualCheck = 1
    )
    co2Scrubber = findLifeSupportRating(
      diagnostics,
      indexToCheck = 0,
      filterEqualCheck = 0
    )
    lifeSup = lifeSupportRating(oxygen, co2Scrubber)
    _ <- printLine(lifeSup)
  } yield ()

  @tailrec
  def findLifeSupportRating(
      diagnostics: Vector[DiagnosticReportLine],
      indexToCheck: Int,
      filterEqualCheck: Int
  ): BinaryNumberAsSeq = {

    if (diagnostics.size == 1) {
      diagnostics.head.numbers
    } else {

      val ones = countHowManyBitsAreOne(indexToCheck, diagnostics)
      val zeros = diagnostics.size - ones
      if (ones >= zeros) {
        findLifeSupportRating(
          diagnostics.filter(_.numbers(indexToCheck) == filterEqualCheck),
          indexToCheck + 1,
          filterEqualCheck
        )
      } else
        findLifeSupportRating(
          diagnostics.filter(
            _.numbers(indexToCheck) == Math.abs(filterEqualCheck - 1)
          ),
          indexToCheck + 1,
          filterEqualCheck
        )
    }
  }

  private def countHowManyBitsAreOne(
      index: Int,
      diagnostics: Vector[DiagnosticReportLine]
  ): Int = {
    diagnostics.map(_.numbers(index)).count(_ == 1)
  }

  private def lifeSupportRating(
      oxygen: BinaryNumberAsSeq,
      co2Scrubber: BinaryNumberAsSeq
  ): Int = {
    Integer.parseInt(oxygen.mkString, 2) *
      Integer.parseInt(co2Scrubber.mkString, 2)
  }

}
