package zio.parser.internal

import zio.parser.Parser.ErasedParser
import zio.parser.{Regex, Parser}

object Debug {

  case class DebugPrinterState(indent: Int, visited: Map[ErasedParser, Int], lastId: Int) {
    def visit(node: ErasedParser): DebugPrinterState =
      copy(visited = this.visited + (node -> lastId), lastId = this.lastId + 1)
    def in: DebugPrinterState                        =
      copy(indent = this.indent + 1)

    def mergeVisited(other: DebugPrinterState): DebugPrinterState =
      copy(
        visited = this.visited ++ other.visited,
        lastId = Math.max(this.lastId, other.lastId)
      )
  }

  private val initialState: DebugPrinterState = DebugPrinterState(0, Map.empty, 0)

  /** Prints a parser tree
    *
    * Useful for debugging constructed parsers.
    */
  def printParserTree(syntax: ErasedParser, state: DebugPrinterState = initialState): DebugPrinterState = {
    implicit val st: DebugPrinterState = state
    state.visited.get(syntax) match {
      case Some(id) =>
        printIndented(s"#[$id]")
        state
      case None     =>
        syntax match {
          case Parser.Lazy(inner)                          =>
            printIndented("Lazy")
            printParserTree(inner(), state.visit(syntax).in)
          case Parser.Succeed(value)                       =>
            printIndented(s"Succeed($value")
            state
          case Parser.Fail(failure)                        =>
            printIndented(s"Fail($failure")
            state
          case Parser.Failed(failure)                      =>
            printIndented(s"Failed($failure")
            state
          case Parser.Named(inner, name)                   =>
            printIndented(s"Named($name)")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.SkipRegex(regex, onFailure)          =>
            printIndented(s"SkipRegex($onFailure)")
            printRegexTree(regex, state.visit(syntax).in)
            state
          case Parser.ParseRegex(regex, onFailure)         =>
            printIndented(s"ParseRegex($onFailure)")
            printRegexTree(regex, state.visit(syntax).in)
            state
          case Parser.ParseRegexLastChar(regex, onFailure) =>
            printIndented(s"ParseRegexLastChar($onFailure)")
            printRegexTree(regex, state.visit(syntax).in)
            state
          case Parser.TransformEither(inner, to)           =>
            printIndented(s"TransformEither")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Transform(inner, to)                 =>
            printIndented(s"Transform")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Ignore(inner, to)                    =>
            printIndented(s"Ignore($to)")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.CaptureString(inner)                 =>
            printIndented("CaptureString")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Zip(left, right, _)                  =>
            printIndented(s"Zip")
            val leftSt = printParserTree(left, state.visit(syntax).in)
            printParserTree(right, state.mergeVisited(leftSt).in)
          case Parser.ZipLeft(left, right)                 =>
            printIndented(s"ZipLeft")
            val leftSt = printParserTree(left, state.visit(syntax).in)
            printParserTree(right, state.mergeVisited(leftSt).in)
          case Parser.ZipRight(left, right)                =>
            printIndented(s"ZipRight")
            val leftSt = printParserTree(left, state.visit(syntax).in)
            printParserTree(right, state.mergeVisited(leftSt).in)
          case Parser.FlatMap(inner, f)                    =>
            printIndented(s"FlatMap")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.OrElseEither(left, right)            =>
            printIndented(s"OrElseEither")
            val leftSt = printParserTree(left, state.visit(syntax).in)
            printParserTree(right, state.mergeVisited(leftSt).in)
          case Parser.OrElse(left, right)                  =>
            printIndented(s"OrElse")
            val leftSt = printParserTree(left, state.visit(syntax).in)
            printParserTree(right, state.mergeVisited(leftSt).in)
          case Parser.Optional(inner)                      =>
            printIndented(s"Optional")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Repeat(inner, min, max)              =>
            printIndented(s"Repeat($min, $max)")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Backtrack(inner)                     =>
            printIndented(s"Backtrack")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.SetAutoBacktrack(inner, enabled)     =>
            printIndented(s"SetAutoBacktrack($enabled)")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.MapError(inner, mapParserErr)        =>
            printIndented(s"MapError")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Not(inner, failure)                  =>
            printIndented("Not")
            printParserTree(inner, state.visit(syntax).in)
          case Parser.Index                                =>
            printIndented("Index")
            state
          case Parser.End                                  =>
            printIndented("End")
            state
        }
    }
  }

  private def printRegexTree(regex: Regex, state: DebugPrinterState): DebugPrinterState = {
    implicit val st: DebugPrinterState = state
    regex match {
      case Regex.Succeed                 =>
        printIndented("<Succeed>")
        state
      case Regex.OneOf(bitset)           =>
        printIndented("<OneOf>")
        state
      case Regex.Sequence(first, second) =>
        printIndented("<Sequence>")
        printRegexTree(first, state.in)
        printRegexTree(second, state.in)
      case Regex.Repeat(regex, min, max) =>
        printIndented(s"<Repeat($min, $max)>")
        printRegexTree(regex, state.in)
      case Regex.Or(left, right)         =>
        printIndented("<Or>")
        printRegexTree(left, state.in)
        printRegexTree(right, state.in)
      case Regex.And(left, right)        =>
        printIndented("<And>")
        printRegexTree(left, state.in)
        printRegexTree(right, state.in)
    }
  }

  private def printIndented(str: String)(implicit state: DebugPrinterState): Unit =
    println(s"[${state.lastId}] " + ("  " * state.indent) + str)
}
