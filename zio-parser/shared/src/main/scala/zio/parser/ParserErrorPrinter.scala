package zio.parser

import zio.parser.Parser.ParserError
import zio.{Console, Ref, ZIO}

import java.io.IOException

final case class ParserErrorPrinter[Err](
    console: Console,
    width: Int,
    input: String,
    error: ParserError[Err],
    ranges: Ref[Map[Range, List[ParserErrorPrinter.Entry]]]
) {

  private val lines      = input.linesWithSeparators.toVector
  private val lineStarts = lines.scanLeft(0)(_ + _.length)

  def prettyPrintFailure(): ZIO[Any, IOException, Unit] =
    gatherPrintPlan(error) *>
      printPlan()

  private def gatherPrintPlan(error: ParserError[Err]): ZIO[Any, Nothing, Unit] =
    error match {
      case ParserError.Failure(nameStack, position, failure) =>
        addErrorLine(position, failure.toString, nameStack)
      case ParserError.UnknownFailure(nameStack, position)   =>
        addErrorLine(position, "unknown failure", nameStack)
      case ParserError.UnexpectedEndOfInput(nameStack)       =>
        addErrorLine(input.length - 1, "unexpected end of input", nameStack)
      case ParserError.NotConsumedAll(position, lastFailure) =>
        addErrorLine(position, "not consumed the whole input", List.empty) *>
          (lastFailure match {
            case Some(failure) =>
              gatherPrintPlan(failure)
            case None          => ZIO.unit
          })
      case ParserError.AllBranchesFailed(left, right)        =>
        (left, right) match {
          case (ParserError.UnexpectedEndOfInput(_), other) =>
            gatherPrintPlan(other)
          case (other, ParserError.UnexpectedEndOfInput(_)) =>
            gatherPrintPlan(other)
          case _                                            =>
            gatherPrintPlan(left) *> gatherPrintPlan(right)
        }
    }

  private def printPlan(): ZIO[Any, IOException, Unit] =
    ranges.get.flatMap { ranges =>
      val sorted = ranges.toList.sortBy { case (rng, _) => rng.start }
      ZIO.foreachDiscard(sorted) { case (rng, entries) =>
        val (rngRow, rngCol) = toCoords(rng.start)
        val section          = input.substring(Math.max(0, rng.start), Math.min(input.length, rng.end)).stripTrailing()
        val orderedEntries   = entries.sortBy(entry => -entry.position)

        val leftPad = if (lines.length == 1) {
          "➡️ "
        } else {
          s"[$rngRow] ➡️ "
        }

        console.printLine(
          leftPad +
            scala.Console.GREEN +
            section +
            scala.Console.RESET
        ) *>
          ZIO.foreachDiscard(orderedEntries) { entry =>
            val (entryRow, entryCol) = toCoords(entry.position)
            val pad                  = " " * (entryCol + leftPad.length)
            console.printLine(
              pad + "\uD83D\uDD34 " +
                scala.Console.RED +
                entry.message +
                (if (entry.stack.nonEmpty)
                   "\u001b[38;2;85;85;85m" +
                     " (" +
                     scala.Console.YELLOW +
                     entry.stack.head +
                     "\u001b[38;2;85;85;85m/" +
                     entry.stack.tail.mkString("/") + ")"
                 else "") + scala.Console.RESET
            )
          }
      }
    }

  private def addErrorLine(position: Int, message: String, stack: List[String]): ZIO[Any, Nothing, Unit] =
    ranges.update { ranges =>
      ranges.keySet.find(_.contains(position)) match {
        case Some(rng) =>
          ranges.get(rng) match {
            case Some(lst) =>
              ranges + (rng -> (ParserErrorPrinter.Entry(position, message, stack) :: lst))
            case None      =>
              ranges + (rng -> List(ParserErrorPrinter.Entry(position, message, stack)))
          }
        case None      =>
          val (row, _) = toCoords(position)
          val rowStart = lineStarts(row)
          val rowEnd   = rowStart + lines(row).length

          val rng = (Math.max(rowStart, position - width / 2)) to (Math.min(rowEnd, position + width / 2))
          ranges + (rng -> List(ParserErrorPrinter.Entry(position, message, stack)))
      }
    }

  private def toCoords(position: Int): (Int, Int) =
    if (lines.length == 1) (0, position)
    else {
      val row = lineStarts.zipWithIndex
        .find { case (col, _) => col > position }
        .map { case (_, idx) => idx - 1 }
        .getOrElse(0)
      (row, position - lineStarts(row))
    }
}

object ParserErrorPrinter {
  final case class Entry(position: Int, message: String, stack: List[String])

  implicit class ParserErrorOps[Err](error: ParserError[Err]) {
    def prettyPrint(input: String, width: Int = 120): ZIO[Console, IOException, Unit] =
      for {
        ranges  <- Ref.make(Map.empty[Range, List[ParserErrorPrinter.Entry]])
        console <- ZIO.service[Console]
        printer  = new ParserErrorPrinter[Err](console, width, input, error, ranges)
        _       <- printer.prettyPrintFailure()
      } yield ()
  }
}
