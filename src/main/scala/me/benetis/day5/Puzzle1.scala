package me.benetis.day5

import zio.Console._
import zio._

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object Puzzle1 extends ZIOAppDefault {

  case class Point(x: Int, y: Int)
  case class VentRange(p1: Point, p2: Point)

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    _ <- printLine("Begin..")
    lines <- ZIO.acquireReleaseWith(
      Task(Source.fromResource("input_day5.txt")),
      (buff: BufferedSource) => UIO(buff.close()),
      (buff: BufferedSource) => Task(buff.getLines().toVector)
    )
    ranges <- Task(lines.map(parseHydrothermalVentRange))
    filteredRanges <- Task(ranges.filter(r => isDiagonal(r) || isVerticalOrHorizonal(r)))
    _ <- printLine("result")
  } yield ()

  def generatePoints(ventRange: VentRange): Vector[Point] = {
    val xRange = Range.inclusive(ventRange.p1.x, ventRange.p2.x)
    val yRange = Range.inclusive(ventRange.p1.y, ventRange.p2.y)
    xRange.toVector.zip(yRange.toVector).map { case(x, y) => Point(x, y)}
  }

  /* https://en.wikipedia.org/wiki/Linear_equation#Two-point%20form */
  def isDiagonal(ventRange: VentRange): Boolean =
    Math.abs(ventRange.p1.x - ventRange.p2.x) == Math.abs(ventRange.p1.y - ventRange.p2.y)

  def isVerticalOrHorizonal(ventRange: VentRange): Boolean =
    ventRange.p1.x == ventRange.p2.x ||
      ventRange.p1.y == ventRange.p2.y

  def parseHydrothermalVentRange(line: String): VentRange = {
    def parseCoords(coords: String): Point = {
      val arr = coords.split(",").filter(_.nonEmpty).map(_.toInt)
      Point(arr(0), arr(1))
    }

    val arrPoints = line.split("->").map(_.trim).map(parseCoords)
    VentRange(arrPoints(0), arrPoints(1))
  }

}
