package me.benetis.day3

import zio.Console._
import zio._

object Puzzle1 extends ZIOAppDefault {

  case class DiagnosticReportLine(numbers: Vector[Byte])
  case class OneZeroIndexed(indexedCounts: IndexedSeq[Byte])
  type BinaryNumberAsSeq = IndexedSeq[Byte]

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    _ <- printLine("Begin..")
    diagnostics = parseInput(InputDay3.input)
    gamma = calculateGamma(diagnostics)
    eps = calculateEpsilon(diagnostics)
    powerConsumption = calculatePowerConsumption(
      binaryVectorToInt(gamma),
      binaryVectorToInt(eps)
    )
    _ <- printLine(powerConsumption)
  } yield ()

  def binaryVectorToInt(binary: BinaryNumberAsSeq): Int =
    Integer.parseInt(binary.mkString, 2)

  def calculateGamma(
      diagnostics: Vector[DiagnosticReportLine]
  ): BinaryNumberAsSeq = {
    calculate(
      diagnostics,
      (ones, zeros) => {
        if (ones >= zeros)
          1.toByte
        else
          0.toByte
      }
    )
  }

  def calculateEpsilon(
      diagnostics: Vector[DiagnosticReportLine]
  ): BinaryNumberAsSeq = {
    calculate(
      diagnostics,
      (ones, zeros) => {
        if (ones <= zeros)
          1.toByte
        else
          0.toByte
      }
    )
  }

  def calculate(
      diagnostics: Vector[DiagnosticReportLine],
      compareFn: (Int, Int) => Byte
  ): IndexedSeq[Byte] = {
    val length = diagnostics.head.numbers.size

    (0 until length).map(i => {
      val indexed = extractNthOneAndZeros(i, diagnostics)
      val ones = indexed.indexedCounts.count(_ == 1)
      val zeros = indexed.indexedCounts.size - ones

      compareFn(ones, zeros)
    })

  }

  private def calculatePowerConsumption(gamma: Int, eps: Int) = gamma * eps

  private def extractNthOneAndZeros(
      index: Int,
      diagnostics: Vector[DiagnosticReportLine]
  ): OneZeroIndexed = {
    OneZeroIndexed(diagnostics.map(_.numbers(index)))
  }

  def parseInput(input: String): Vector[DiagnosticReportLine] = {
    /* Assume input is good */
    input
      .split("[\n\r]")
      .filter(_.nonEmpty)
      .map(str =>
        DiagnosticReportLine(
          str.split("").map(_.toByte).toVector
        )
      )
      .toVector
  }

}
