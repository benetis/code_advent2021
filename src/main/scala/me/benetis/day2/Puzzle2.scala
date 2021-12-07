package me.benetis.day2

import me.benetis.day1.Puzzle1.Depth
import zio.Console._
import zio._

import scala.util.Try

//noinspection DuplicatedCode
object Puzzle2 extends ZIOAppDefault {

  case class Aim(value: Int)

  sealed trait CommandDirection
  case class Forward(value: Int) extends CommandDirection
  case class Down(value: Int) extends CommandDirection
  case class Up(value: Int) extends CommandDirection

  case class CurrentState(depth: Depth, aim: Aim, horizontalPosition: Int)

  override def run: ZIO[ZEnv with Has[ZIOAppArgs], Any, Any] = for {
    parsed <- parseInput(InputDay2.input)
    position = executeCommands(parsed, CurrentState(Depth(0), Aim(0), 0))
    multiplied = position.depth.value * position.horizontalPosition
    _ <- printLine(multiplied)
  } yield ()

  def executeCommands(
      commands: IndexedSeq[CommandDirection],
      startAt: CurrentState
  ): CurrentState = {
    commands.foldLeft(startAt)((prevPos, curr) => {
      curr match {
        case Forward(value) =>
          prevPos.copy(
            horizontalPosition = prevPos.horizontalPosition + value,
            depth = Depth(prevPos.depth.value + (prevPos.aim.value * value))
          )
        case Down(value) =>
          prevPos.copy(
            aim = Aim(prevPos.aim.value + value)
          )
        case Up(value) =>
          prevPos.copy(
            aim = Aim(prevPos.aim.value - value)
          )
      }
    })
  }

  def parseInput(input: String): Task[IndexedSeq[CommandDirection]] = {
    ZIO
      .foreach {
        input
          .split("[\n\r]")
          .filter(_.nonEmpty)
      }(parseLine)
      .map(_.toIndexedSeq)
  }

  private def parseLine(line: String): Task[CommandDirection] = {
    val Array(directionStr: String, amountStr: String) = line.split(" ")

    val moveAmount = ZIO.fromTry(Try(amountStr.toInt))

    moveAmount.flatMap(amount =>
      directionStr match {
        case "forward" => ZIO.succeed(Forward(amount))
        case "up"      => ZIO.succeed(Up(amount))
        case "down"    => ZIO.succeed(Down(amount))
        case _         => ZIO.fail(new Exception("unsupported direction"))
      }
    )
  }

}
