package zio.parser

import zio.parser.Parser.{ErasedParser, ParserError}
import zio.parser.internal.recursive.ParserState
import zio.parser.internal.recursive.ParserState.StateSelector
import zio.parser.internal.stacksafe.ParserOp.InitialParser
import zio.parser.internal.stacksafe.{CharParserImpl, ParserOp}
import zio.parser.internal.{PZippable, recursive}
import zio.{Chunk, ChunkBuilder}

import scala.annotation.nowarn
import scala.collection.mutable

/** A Parser consumes a stream of 'In's and either fails with a ParserError possibly holding a custom error of type
  * 'Err' or succeeds with a result of type `Result`
  *
  * Parsers can be combined with Printers to get Syntax (`parser <=> printer`), or a Parser and a Printer can be built
  * simultaneously by using the combinators of [[Syntax]].
  *
  * Recursive parsers can be expressed directly by recursing in one of the zipping or or-else combinators, whose
  * argument is passed by name.
  *
  * By default a parser backtracks automatically. This behavior can be changed with the 'manualBacktracking' operator.
  *
  * Parser trees get optimized automatically before running the parser. This optimized tree can be examined by the
  * 'optimized' field.
  *
  * {{{
  * val number: Parser[String, Char, Int]       = Parser.digit.repeat.string.map(_.toInt)
  * val point: Parser[String, Char, (Int, Int)] = number ~ Parser.char(',') ~ number
  *
  * point.parseString("12,34") // Right((12,34))
  * point.parseString("12;34") // Left(StringParserError(12;34,Failure(List(),2,not ',')))
  * }}}
  *
  * @tparam Err
  *   Custom error type
  * @tparam In
  *   Element type of the input stream of parsing
  * @tparam Result
  *   The type of the parsed result value
  * @see
  *   [[https://zio.dev/zio-parser/syntax/ Building Syntaxes]]
  */
sealed trait Parser[+Err, -In, +Result] extends VersionSpecificParser[Err, In, Result] { self =>

  /** Maps the parser's successful result with the given function 'to'
    *
    * {{{
    * Parser.digit.map(_.asDigit).parseString("7") // Right(7)
    * }}}
    */
  final def map[Result2](to: Result => Result2): Parser[Err, In, Result2] =
    Parser.Transform(self, to)

  /** Ignores the parser's successful result and result in 'result' instead */
  final def as[Result2](result: Result2): Parser[Err, In, Result2] =
    Parser.Ignore(self, result)

  /** Maps the parser's successful result with the given function 'to' that either fails or produces a new result value.
    * A `Left` fails the parser with a [[Parser.ParserError.Failure]] holding the value.
    *
    * Currently, failures of this parser itself are passed on unchanged, so they keep their original error value
    * although the error type becomes `Err2`.
    *
    * {{{
    * val year = Parser.digit.repeat.string.transformEither[String, Int] { s =>
    *   if (s.length <= 4) Right(s.toInt) else Left("too long")
    * }
    * year.parseString("12345") // Left(StringParserError(12345,Failure(List(),5,too long)))
    * }}}
    */
  final def transformEither[Err2, Result2](
      to: Result => Either[Err2, Result2]
  ): Parser[Err2, In, Result2] =
    Parser.TransformEither(self, to)

  /** Maps the parser's successful result with the given function 'to' that either produces a new result value or the
    * failure is indicated in the error channel by the value None.
    *
    * Currently, failures of this parser itself are passed on unchanged (not wrapped in `Some`), so they keep their
    * original error value although the error type becomes `Option[Err]`.
    */
  final def transformOption[Result2](
      to: Result => Option[Result2]
  ): Parser[Option[Err], In, Result2] =
    transformEither[Option[Err], Result2](value => to(value).toRight(None))

  /** Symbolic alias for zip */
  final def ~[Err2 >: Err, In2 <: In, Result2, ZippedResult](
      that: => Parser[Err2, In2, Result2]
  )(implicit zippable: PZippable.Out[Result, Result2, ZippedResult]): Parser[Err2, In2, ZippedResult] =
    zip(that)

  /** Concatenates this parser with 'that' parser. In case both parsers succeed, the results are combined: a `Unit`
    * result is dropped, and a result that is already a tuple is extended, so `a ~ b ~ c` results in a flat triple.
    *
    * {{{
    * (Parser.letter ~ Parser.char('=') ~ Parser.digit).parseString("x=5") // Right((x,5))
    * }}}
    */
  final def zip[Err2 >: Err, In2 <: In, Result2, ZippedResult](
      that: => Parser[Err2, In2, Result2]
  )(implicit zippable: PZippable.Out[Result, Result2, ZippedResult]): Parser[Err2, In2, ZippedResult] =
    Parser.Zip(Parser.Lazy(() => self), Parser.Lazy(() => that), zippable.zip)

  /** Symbolic alias for zipLeft */
  final def <~[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Unit]
  ): Parser[Err2, In2, Result] =
    zipLeft(that)

  /** Concatenates this parser with 'that' parser. In case both parsers succeed, the result is the result of this
    * parser. Otherwise the parser fails.
    */
  final def zipLeft[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Any]
  ): Parser[Err2, In2, Result] =
    Parser.ZipLeft(Parser.Lazy(() => self), Parser.Lazy(() => that))

  /** Repeats this parser exactly `n` times. It stops after `n` items and fails if there are fewer.
    *
    * Currently, with [[ParserImplementation.StackSafe]] the upper limit is not applied unless this parser is built with
    * `regexChar` (as `Parser.digit` is): `Parser.digit.map(_.asDigit).exactly(2)` reads all four digits of "1234".
    *
    * {{{
    * Parser.digit.exactly(3).string.parseString("2024") // Right(202)
    * }}}
    */
  final def exactly(n: Int): Parser[Err, In, Chunk[Result]] =
    Parser.Repeat(self, n, Some(n))

  /** Determines the continuation of the parser by the result of this parser, expressed by the function 'that'
    *
    * {{{
    * // the first digit gives the number of characters that follow
    * Parser.digit.flatMap(n => Parser.anyChar.exactly(n.asDigit).string).parseString("3abcd") // Right(abc)
    * }}}
    */
  final def flatMap[Err2 >: Err, In2 <: In, Result2](
      that: Result => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result2] =
    Parser.FlatMap(self, that)

  /** Checks the result of this parser with the given function. If the 'condition' is false, fails with the given
    * failure 'failure', otherwise results in this parser's result.
    *
    * {{{
    * val nonZero = Parser.digit.filter((c: Char) => c != '0', "zero")
    * nonZero.parseString("0") // Left(StringParserError(0,Failure(List(),1,zero)))
    * }}}
    */
  final def filter[Result2, Err2 >: Err](condition: Result2 => Boolean, failure: Err2)(implicit
      ev: Result <:< Result2
  ): Parser[Err2, In, Result2] =
    transformEither((d2: Result) => if (condition(ev(d2))) Right(d2) else Left(failure))

  /** Associates a name with this parser. The chain of named parsers are reported in case of failure to help debugging
    * parser issues: the names are in the `nameStack` of the error, innermost first.
    *
    * {{{
    * (Parser.digit ?? "digit").parseString("x") // Left(StringParserError(x,Failure(List(digit),0,not a digit)))
    * }}}
    */
  final def named(name: String): Parser[Err, In, Result] =
    Parser.Named(self, name)

  /** Symbolic alias for named */
  final def ??(name: String): Parser[Err, In, Result] = named(name)

  /** Symbolic alias for orElse */
  final def |[Err2 >: Err, In2 <: In, Result2 >: Result](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result2] = orElse(that)

  /** Symbolic alias for orElse */
  final def <>[Err2 >: Err, In2 <: In, Result2 >: Result](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result2] = orElse(that)

  /** Assigns 'that' parser as a fallback of this. First this parser gets evaluated. In case it succeeds, the result is
    * this parser's result. In case it fails, the result is 'that' parser's result. If both fail, the error is a
    * [[Parser.ParserError.AllBranchesFailed]] holding both failures.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser. With auto-backtracking off,
    * 'that' is tried only when this parser failed without consuming input; otherwise this parser's failure is the
    * result.
    *
    * {{{
    * val answer = Parser.string("yes", true) | Parser.string("no", false)
    * answer.parseString("no") // Right(false)
    * answer.parseString("maybe")
    * // Left(StringParserError(maybe,AllBranchesFailed(Failure(List(),0,Not 'yes'),Failure(List(),0,Not 'no'))))
    * }}}
    */
  final def orElse[Err2 >: Err, In2 <: In, Result2 >: Result](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result2] =
    Parser.OrElse(Parser.Lazy(() => self), Parser.Lazy(() => that))

  /** Symbolic alias for orElseEither */
  final def <+>[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Either[Result, Result2]] =
    orElseEither(that)

  /** Assigns 'that' parser as a fallback of this. First this parser gets evaluated. In case it succeeds, the result is
    * this parser's result wrapped in 'Left'. In case it fails, the result is 'that' parser's result, wrapped in
    * 'Right'.
    *
    * Compared to orElse, this version allows the two parsers to have different result types.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser. With auto-backtracking off,
    * 'that' is tried only when this parser failed without consuming input.
    *
    * {{{
    * (Parser.digit <+> Parser.letter).parseString("k") // Right(Right(k))
    * }}}
    */
  final def orElseEither[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Either[Result, Result2]] =
    Parser.OrElseEither(Parser.Lazy(() => self), Parser.Lazy(() => that))

  /** Repeats this parser at least 'min' times.
    *
    * The result is all the parsed elements until the first failure. The failure that stops the repetition gets
    * swallowed and in case auto-backtracking is on, the parser backtracks to the end of the last successful item. If
    * fewer than 'min' elements were parsed, the parser fails.
    *
    * {{{
    * Parser.letter.atLeast(2).parseString("ok!") // Right(Chunk(o,k))
    * }}}
    */
  final def atLeast(min: Int): Parser[Err, In, Chunk[Result]] =
    Parser.Repeat(self, min, None)

  /** Repeats this parser at least once.
    *
    * The result is all the parsed elements until the first failure. The failure that stops the repetition gets
    * swallowed and in case auto-backtracking is on, the parser backtracks to the end of the last successful item.
    *
    * {{{
    * Parser.digit.repeat.parseString("2024-01") // Right(Chunk(2,0,2,4))
    * }}}
    */
  final def repeat: Parser[Err, In, Chunk[Result]] =
    atLeast(1)

  /** Symbolic alias for repeat */
  final def + : Parser[Err, In, Chunk[Result]] = repeat

  /** Repeats this parser zero or more times.
    *
    * The result is all the parsed elements until the first failure. The failure that stops the repetition gets
    * swallowed and in case auto-backtracking is on, the parser backtracks to the end of the last successful item.
    */
  final def repeat0: Parser[Err, In, Chunk[Result]] =
    atLeast(0)

  /** Symbolic alias for repeat0 */
  final def * : Parser[Err, In, Chunk[Result]] = repeat0

  /** Repeats this parser at least once and requires that between each element, the 'sep' parser succeeds
    *
    * {{{
    * Parser.digit.repeatWithSep(Parser.char(',')).parseString("1,2,3") // Right(Chunk(1,2,3))
    * }}}
    */
  final def repeatWithSep[Err2 >: Err, In2 <: In](
      sep: Parser[Err2, In2, Unit]
  ): Parser[Err2, In2, Chunk[Result]] =
    (self ~ (sep ~> self).repeat0).map { case (head, tail) => head +: tail }

  /** Repeats this parser zero or more times and requires that between each element, the 'sep' parser succeeds */
  final def repeatWithSep0[Err2 >: Err, In2 <: In](
      sep: Parser[Err2, In2, Unit]
  ): Parser[Err2, In2, Chunk[Result]] =
    (self ~ (sep ~> self).repeat0).optional
      .map {
        case Some((head, tail)) => head +: tail
        case None               => Chunk.empty
      }

  /** Repeats this parser until the given `stopCondition` parser succeeds. The input matched by `stopCondition` is
    * consumed but not part of the result. Reaching the end of the input also ends the repetition. Runs with
    * auto-backtracking off.
    *
    * {{{
    * val key = Parser.anyChar.repeatUntil(Parser.char(';'))
    * (key ~ Parser.anyString).parseString("key;rest") // Right((Chunk(k,e,y),rest))
    * }}}
    */
  final def repeatUntil[Err2 >: Err, In2 <: In](
      stopCondition: Parser[Err2, In2, Any]
  ): Parser[Err2, In2, Chunk[Result]] =
    (stopCondition.not(null.asInstanceOf[Err2]) ~> self).repeat0.manualBacktracking

  /** Make this parser optional.
    *
    * A failure of this parser results in `None`. In case auto-backtracking is enabled, backtracking is performed on it.
    * With auto-backtracking off, a failure after consuming input fails the optional parser too.
    *
    * {{{
    * (Parser.char('-').optional ~ Parser.digit).parseString("7") // Right((None,7))
    * }}}
    */
  final def optional: Parser[Err, In, Option[Result]] =
    Parser.Optional(self)

  /** Symbolic alias for optional */
  final def ? : Parser[Err, In, Option[Result]] = optional

  /** Concatenates the parsers 'left', then this, then 'right'.
    *
    * All three must succeed. The result is this parser's result.
    *
    * {{{
    * Parser.digit.between(Parser.char('['), Parser.char(']')).parseString("[4]") // Right(4)
    * }}}
    */
  final def between[Err2 >: Err, In2 <: In](
      left: Parser[Err2, In2, Any],
      right: Parser[Err2, In2, Any]
  ): Parser[Err2, In2, Result] =
    (left ~ self ~ right).map { case (_, value, _) => value }

  /** Surrounds this parser with the 'other' parser. The result is this parser's result. */
  final def surroundedBy[Err2 >: Err, In2 <: In](
      other: Parser[Err2, In2, Any]
  ): Parser[Err2, In2, Result] =
    (other ~ self ~ other).map { case (_, value, _) => value }

  /** Maps the custom error values with the given function 'f'. Only [[Parser.ParserError.Failure]] errors hold one
    * (also inside [[Parser.ParserError.AllBranchesFailed]]); the other cases are kept as they are.
    *
    * Currently, with [[ParserImplementation.StackSafe]] a successful parse throws a `java.lang.NullPointerException`.
    *
    * {{{
    * Parser.digit.mapError(msg => msg.length).parseString("x") // Left(StringParserError(x,Failure(List(),0,11)))
    * }}}
    */
  final def mapError[Err2](f: Err => Err2): Parser[Err2, In, Result] =
    Parser.MapError[Err, Err2, In, Result](
      self,
      _.map(f)
    )

  /** Ignores this parser's result and instead capture the parsed string fragment
    *
    * {{{
    * Parser.letter.repeat.string.parseString("abc1") // Right(abc)
    * }}}
    */
  final def string(implicit ev: Char <:< In): Parser[Err, Char, String] =
    Parser.CaptureString[Err, Err](self.asInstanceOf[Parser[Err, Char, Result]])

  /** Flattens a result of parsed strings to a single string */
  final def flatten(implicit
      ev2: Result <:< Chunk[String]
  ): Parser[Err, In, String] =
    map(s => s.mkString)

  /** Runs this parser and discards its result, resulting in the unit value. */
  final def unit: Parser[Err, In, Unit] = as(())

  /** Parser that fails with the given 'failure' if this parser succeeds, and succeeds with the unit value if this
    * parser fails. It does not rewind the input: what this parser consumed stays consumed in both cases.
    *
    * {{{
    * val name = Parser.string("end", ()).not("reserved word") ~ Parser.letter.repeat.string
    * name.parseString("name") // Right(name)
    * name.parseString("end")  // Left(StringParserError(end,Failure(List(),3,reserved word)))
    * }}}
    */
  final def not[Err2 >: Err](failure: => Err2): Parser[Err2, In, Unit] =
    Parser.Not(self, failure)

  /** Parser that resets the parsing position in case it fails.
    *
    * By default backtracking points are automatically inserted. This behavior can be changed with the autoBacktracking,
    * manualBacktracking and setAutoBacktracking combinators.
    *
    * {{{
    * val ab = (Parser.char('a') ~ Parser.char('b')).as(1)
    * val ac = (Parser.char('a') ~ Parser.char('c')).as(2)
    * (ab.backtrack | ac).manualBacktracking.parseString("ac") // Right(2)
    * }}}
    */
  final def backtrack: Parser[Err, In, Result] = Parser.Backtrack(self)

  /** Enables auto-backtracking for this parser */
  final def autoBacktracking: Parser[Err, In, Result] =
    setAutoBacktracking(true)

  /** Turns off auto-backtracking for this parser, including the parsers it is built from.
    *
    * {{{
    * val ab = (Parser.char('a') ~ Parser.char('b')).as(1)
    * val ac = (Parser.char('a') ~ Parser.char('c')).as(2)
    * (ab | ac).parseString("ac")                    // Right(2)
    * (ab | ac).manualBacktracking.parseString("ac") // Left(StringParserError(ac,Failure(List(),1,not 'b')))
    * }}}
    */
  final def manualBacktracking: Parser[Err, In, Result] =
    setAutoBacktracking(false)

  /** Enables or disables auto-backtracking for this parser */
  final def setAutoBacktracking(enabled: Boolean): Parser[Err, In, Result] =
    Parser.SetAutoBacktrack(self, enabled)

  // Execution
  /** Run this parser on the given 'input' string, with the default implementation ([[ParserImplementation.Recursive]]).
    * A failure is returned as a [[StringParserError]] that also holds the input.
    *
    * The parser does not have to consume the whole input; end it with `Parser.end` to require that.
    *
    * {{{
    * Parser.string("true", true).parseString("true") // Right(true)
    * Parser.string("true", true).parseString("tree") // Left(StringParserError(tree,Failure(List(),0,Not 'true')))
    * }}}
    */
  final def parseString(input: String)(implicit ev: Char <:< In): Either[StringParserError[Err], Result] =
    parseString(input, self.defaultImplementation)

  /** Run this parser on the given 'input' string using a specific parser implementation */
  final def parseString(input: String, parserImplementation: ParserImplementation)(implicit
      ev: Char <:< In
  ): Either[StringParserError[Err], Result] =
    parserImplementation match {
      case ParserImplementation.StackSafe =>
        val parser = new CharParserImpl[Err, Result](
          self.compiledOpStack,
          input
        )
        parser.run().left.map(err => StringParserError(input, err))
      case ParserImplementation.Recursive =>
        val state  = recursive.ParserState.fromString(input)
        val result = self.optimized.parseRec(state.asInstanceOf[ParserState[In]])

        if (state.error != null)
          Left(StringParserError(input, state.error.asInstanceOf[ParserError[Err]]))
        else
          Right(result)
    }

  /** Run this parser on the given 'input' chunk of characters */
  final def parseChars(input: Chunk[Char])(implicit ev: Char <:< In): Either[StringParserError[Err], Result] =
    parseChars(input, self.defaultImplementation)

  /** Run this parser on the given 'input' chunk of characters using a specific parser implementation */
  final def parseChars(input: Chunk[Char], parserImplementation: ParserImplementation)(implicit
      ev: Char <:< In
  ): Either[StringParserError[Err], Result] =
    parseString(new String(input.toArray), parserImplementation)

  /** Run this parser on the given 'input' chunk, with the [[ParserImplementation.Recursive]] implementation. Unlike
    * `parseString`, it accepts any element type and returns the [[Parser.ParserError]] without the input. Regex-based
    * parsers need a `Chunk[Char]`; on other element types they throw a `java.lang.UnsupportedOperationException`.
    *
    * {{{
    * import zio.Chunk
    *
    * val positives = Parser.any[Int].filter((i: Int) => i > 0, "not positive").repeat
    * positives.parseChunk(Chunk(3, 1, 4)) // Right(Chunk(3,1,4))
    * }}}
    */
  final def parseChunk[In0 <: In](
      input: Chunk[In0]
  )(implicit stateSelector: StateSelector[In0]): Either[ParserError[Err], Result] = {
    val state  = recursive.ParserState.fromChunk(input)
    val result = self.optimized.parseRec(state)

    if (state.error != null)
      Left(state.error.asInstanceOf[ParserError[Err]])
    else
      Right(result)
  }

  /** The optimized parser tree used by the parser implementations */
  lazy val optimized: Parser[Err, In, Result] = {
    val optimizedNodes = Parser.OptimizerState(
      mutable.Map.empty,
      mutable.Map.empty,
      autoBacktrack = true // Default auto backtrack state
    )
    runOptimizeNode(optimizedNodes)
  }

  protected def optimizeNode(
      state: Parser.OptimizerState
  ): Parser[Err, In, Result]

  protected def runOptimizeNode(
      optimizerState: Parser.OptimizerState
  ): Parser[Err, In, Result] =
    optimizerState.optimized.get(self) match {
      case Some(alreadyOptimized) =>
        alreadyOptimized.asInstanceOf[Parser[Err, In, Result]]
      case None                   =>
        optimizerState.visited.update(self, optimizerState.visited.getOrElse(self, 0) + 1)
        val optimized = optimizeNode(optimizerState)
        optimizerState.optimized.put(self, optimized)
        optimized
    }

  /** Strips all the name information from this parser to improve performance but reduces the failure message's
    * verbosity.
    *
    * Currently, names inside the right-hand side of a `zip` (`~`) are kept.
    */
  def strip: Parser[Err, In, Result] = {
    val strippedNodes = Parser.OptimizerState(
      mutable.Map.empty,
      mutable.Map.empty,
      autoBacktrack = true
    )
    runStripNode(strippedNodes)
  }

  protected def stripNode(
      state: Parser.OptimizerState
  ): Parser[Err, In, Result]

  protected def runStripNode(
      stripState: Parser.OptimizerState
  ): Parser[Err, In, Result] =
    stripState.optimized.get(self) match {
      case Some(alreadyOptimized) =>
        alreadyOptimized.asInstanceOf[Parser[Err, In, Result]]
      case None                   =>
        stripState.visited.update(self, stripState.visited.getOrElse(self, 0) + 1)
        val strippedNode = stripNode(stripState)
        stripState.optimized.put(self, strippedNode)
        strippedNode
    }

  /** The operations that [[ParserImplementation.StackSafe]] runs, compiled from `optimized`. */
  lazy val compiledOpStack: InitialParser = ParserOp.compile(optimized.asInstanceOf[ErasedParser])

  /** The implementation used by `parseString` and `parseChars` when none is given: [[ParserImplementation.Recursive]].
    */
  lazy val defaultImplementation: ParserImplementation =
    // NOTE: here we can analyse the parser tree to select an implementation (for example check if it has FlatMap)
    ParserImplementation.Recursive

  protected def parseRec(state: recursive.ParserState[In]): Result

  protected def needsBacktrack: Boolean
}

/** Constructors of [[Parser]] values, and the parser node types that the combinators build.
  *
  * {{{
  * val keyValue = Parser.letter.repeat.string ~ Parser.char('=') ~ Parser.digit.repeat.string
  * keyValue.parseString("port=8080") // Right((port,8080))
  * }}}
  */
object Parser {
  private[parser] type ErasedParser = Parser[_, _, _]

  private[parser] case class OptimizerState(
      optimized: mutable.Map[Parser[_, _, _], Parser[_, _, _]],
      visited: mutable.Map[Parser[_, _, _], Int],
      autoBacktrack: Boolean
  )

  /** A parser that builds its inner parser on first use; the zipping and or-else combinators use it for their
    * arguments, which makes recursive parsers possible.
    */
  final case class Lazy[+Err, -In, +Result](inner: () => Parser[Err, In, Result]) extends Parser[Err, In, Result] {

    /** The inner parser, built once. */
    lazy val memoized: Parser[Err, In, Result] = inner()

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] =
      if (state.visited(this) > 1) {
        Lazy(() => state.optimized(this).asInstanceOf[Parser[Err, In, Result]])
      } else {
        memoized.runOptimizeNode(state)
      }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Result] =
      if (state.visited(this) > 1) {
        Lazy(() => state.optimized(memoized).asInstanceOf[Parser[Err, In, Result]])
      } else {
        memoized.runStripNode(state)
      }

    override protected def parseRec(state: ParserState[In]): Result =
      memoized.parseRec(state)

    override protected lazy val needsBacktrack: Boolean = true
  }

  /** The parser built by `Parser.succeed`. */
  final case class Succeed[+Result](value: Result) extends Parser[Nothing, Any, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Nothing, Any, Result] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Nothing, Any, Result] = this

    override protected def parseRec(state: ParserState[Any]): Result =
      value

    override protected val needsBacktrack: Boolean = false
  }

  /** The parser built by `Parser.fail`. */
  final case class Fail[+Err](failure: Err) extends Parser[Err, Any, Any] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, Any, Any] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, Any, Any] = this

    override protected def parseRec(state: ParserState[Any]): Any = {
      state.error = ParserError.Failure(state.nameStack, state.position, failure)
      null
    }

    override protected val needsBacktrack: Boolean = false
  }

  /** A parser that fails with the given [[Parser.ParserError]] without consuming input. */
  final case class Failed[+Err](failure: ParserError[Err]) extends Parser[Err, Any, Nothing] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def parseRec(state: ParserState[Any]): Nothing = {
      state.error = failure
      null.asInstanceOf[Nothing]
    }

    override protected val needsBacktrack: Boolean = false
  }

  /** The parser built by `named`. */
  final case class Named[Err, In, Result](parser: Parser[Err, In, Result], name: String)
      extends Parser[Err, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = Named(parser.runOptimizeNode(state), name)

    override protected def stripNode(state: OptimizerState): Parser[Err, In, Result] =
      parser.runStripNode(state)

    override protected def parseRec(state: ParserState[In]): Result = {
      state.pushName(name)
      val result = parser.parseRec(state)
      state.popName()
      result
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by `Parser.any`. */
  final case class Passthrough[-D1, +D2]() extends Parser[Nothing, D1, D2] {
    override protected def optimizeNode(state: OptimizerState): Parser[Nothing, D1, D2] = Passthrough()

    override protected def stripNode(state: OptimizerState): Parser[Nothing, D1, D2] = Passthrough()

    override protected def parseRec(state: ParserState[D1]): D2 =
      if (state.position < state.length) {
        val result = state.at(state.position)
        state.position += 1
        result.asInstanceOf[D2]
      } else {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
        null.asInstanceOf[D2]
      }

    override protected def needsBacktrack: Boolean = false
  }

  /** A parser that matches a regex and discards the matched input; built by `Parser.regexDiscard` and
    * `Parser.unsafeRegexDiscard`.
    */
  final case class SkipRegex[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Unit] {

    /** The regex compiled for matching, built on first use. */
    lazy val compiledRegex: Regex.Compiled = regex.compile

    override protected def optimizeNode(state: OptimizerState): Parser[Err, Char, Unit] = {
      this.compiledRegex.test(0, "")
      this
    }

    override protected def stripNode(state: OptimizerState): Parser[Err, Char, Unit] = this

    override protected def parseRec(state: ParserState[Char]): Unit = {
      val position = state.position
      val result   = state.regex(compiledRegex)
      if (result == Regex.NeedMoreInput) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
      } else if (result == Regex.NotMatched) {
        onFailure match {
          case Some(failure) =>
            state.error = ParserError.Failure(state.nameStack, position, failure)
          case None          =>
            state.error = ParserError.UnknownFailure(state.nameStack, position)
        }
      } else {
        state.position = result
      }
    }

    override protected val needsBacktrack: Boolean = false

    override def toString: String = "ParseRegex()"
  }

  /** A parser that matches a regex and results in the matched characters; built by `Parser.regex` and
    * `Parser.unsafeRegex`.
    */
  final case class ParseRegex[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Chunk[Char]] {

    /** The regex compiled for matching, built on first use. */
    lazy val compiledRegex: Regex.Compiled = regex.compile

    private val getFailure: (Int, List[String]) => ParserError[Err] =
      onFailure match {
        case Some(failure) =>
          (position: Int, nameStack: List[String]) => ParserError.Failure(nameStack, position, failure)
        case None          =>
          (position: Int, nameStack: List[String]) => ParserError.UnknownFailure(nameStack, position)
      }

    override protected def optimizeNode(state: OptimizerState): Parser[Err, Char, Chunk[Char]] = {
      this.compiledRegex.test(0, "")
      this
    }

    override protected def stripNode(state: OptimizerState): Parser[Err, Char, Chunk[Char]] = this

    override protected def parseRec(state: ParserState[Char]): Chunk[Char] = {
      val position = state.position
      val result   = state.regex(compiledRegex)
      if (result == Regex.NeedMoreInput) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
        null.asInstanceOf[Chunk[Char]]
      } else if (result == Regex.NotMatched) {
        state.error = getFailure(position, state.nameStack)
        null.asInstanceOf[Chunk[Char]]
      } else {
        state.position = result
        if (!state.discard) {
          state.sliceToChunk(position, result)
        } else null
      }
    }

    override protected val needsBacktrack: Boolean = false

    override def toString: String = "ParseRegex()"
  }

  /** A parser that matches a regex and results in the last matched character; built by `Parser.regexChar` and
    * `Parser.unsafeRegexChar`.
    */
  final case class ParseRegexLastChar[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Char] {

    /** The regex compiled for matching, built on first use. */
    lazy val compiledRegex: Regex.Compiled = regex.compile

    private val getFailure: (Int, List[String]) => ParserError[Err] =
      onFailure match {
        case Some(failure) =>
          (position: Int, nameStack: List[String]) => ParserError.Failure(nameStack, position, failure)
        case None          =>
          (position: Int, nameStack: List[String]) => ParserError.UnknownFailure(nameStack, position)
      }

    override protected def optimizeNode(state: OptimizerState): Parser[Err, Char, Char] = {
      this.compiledRegex.test(0, "")
      this
    }

    override protected def stripNode(state: OptimizerState): Parser[Err, Char, Char] = this

    override protected def parseRec(state: ParserState[Char]): Char = {
      val position = state.position
      val result   = state.regex(compiledRegex)
      if (result == Regex.NeedMoreInput) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
        null.asInstanceOf[Char]
      } else if (result == Regex.NotMatched) {
        state.error = getFailure(position, state.nameStack)
        null.asInstanceOf[Char]
      } else {
        state.position = result
        if (!state.discard) {
          state.char(result - 1)
        } else null.asInstanceOf[Char]
      }
    }

    override protected val needsBacktrack: Boolean = false

    override def toString: String = "ParseRegexLastChar()"
  }

  /** The parser built by `transformEither`. */
  final case class TransformEither[Err, Err2, In, Result, Result2](
      parser: Parser[Err, In, Result],
      to: Result => Either[Err2, Result2]
  ) extends Parser[Err2, In, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runOptimizeNode(state)
      inner match {
        case TransformEither(parser0, to0) =>
          TransformEither(
            parser0,
            to0.asInstanceOf[Any => Either[Err2, Result]].andThen(_.flatMap(to))
          )
        case Transform(parser0, to0)       =>
          TransformEither(parser0, to0.andThen(to))
        case _                             =>
          TransformEither(inner, to)
      }
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runStripNode(state)
      TransformEither(inner, to)
    }

    override protected def parseRec(state: ParserState[In]): Result2 = {
      // NOTE: cannot skip in discard mode, we need to detect failures
      val discard     = state.discard
      state.discard = false
      val innerResult = parser.parseRec(state)
      state.discard = discard
      if (state.error == null) {
        to(innerResult) match {
          case Left(failure) =>
            state.error = ParserError.Failure(state.nameStack, state.position, failure)
            null.asInstanceOf[Result2]
          case Right(value)  =>
            value
        }
      } else {
        null.asInstanceOf[Result2]
      }
    }

    override protected lazy val needsBacktrack: Boolean = true
  }

  /** The parser built by `map`. */
  final case class Transform[Err, Err2, In, Result, Result2](
      parser: Parser[Err, In, Result],
      to: Result => Result2
  ) extends Parser[Err2, In, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runOptimizeNode(state)
      inner match {
        case TransformEither(parser0, to0) =>
          TransformEither(
            parser0,
            to0.asInstanceOf[Any => Either[Err2, Result]].andThen(_.map(to))
          )
        case Transform(parser0, to0)       =>
          Transform(parser0, to0.andThen(to))
        case _                             =>
          Transform(inner, to)
      }
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runStripNode(state)
      Transform(inner, to)
    }

    override protected def parseRec(state: ParserState[In]): Result2 = {
      val result = parser.parseRec(state)
      if (!state.discard && state.error == null) {
        to(result)
      } else {
        null.asInstanceOf[Result2]
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by `as`. */
  final case class Ignore[Err, Err2, In, Result, Result2](
      parser: Parser[Err, In, Result],
      to: Result2
  ) extends Parser[Err2, In, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runOptimizeNode(state)
      inner match {
        case TransformEither(parser0, to0) =>
          TransformEither(
            parser0,
            to0.asInstanceOf[Any => Either[Err2, Result]].andThen(_.map(_ => to))
          )
        case Transform(parser0, _)         =>
          Ignore(parser0, to)
        case Ignore(parser0, _)            =>
          Ignore(parser0, to)
        case capture: CaptureString[_, _]  =>
          Ignore(capture.parser, to).asInstanceOf[Parser[Err2, In, Result2]]
        case regex: ParseRegex[_]          =>
          Ignore(SkipRegex(regex.regex, regex.onFailure), to).asInstanceOf[Parser[Err2, In, Result2]]
        case regex: ParseRegexLastChar[_]  =>
          Ignore(SkipRegex(regex.regex, regex.onFailure), to).asInstanceOf[Parser[Err2, In, Result2]]
        case _                             =>
          Ignore(inner, to)
      }
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In, Result2] = {
      val inner = parser.runStripNode(state)
      Ignore(inner, to)
    }

    override protected def parseRec(state: ParserState[In]): Result2 = {
      val discard = state.discard
      state.discard = true
      val _       = parser.parseRec(state)
      state.discard = discard
      to
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by `string`. */
  final case class CaptureString[Err, Err2](parser: Parser[Err, Char, Any]) extends Parser[Err2, Char, String] {
    override protected def optimizeNode(state: OptimizerState): Parser[Err2, Char, String] = {
      val inner = parser.runOptimizeNode(state)
      inner match {
        case Transform(parser0, _)                =>
          CaptureString(parser0)
        case Ignore(parser0, _)                   =>
          CaptureString(parser0)
        case CaptureString(parser0)               =>
          CaptureString(parser0)
        case ParseRegex(regex, onFailure)         =>
          CaptureString(SkipRegex(regex, onFailure))
        case ParseRegexLastChar(regex, onFailure) =>
          CaptureString(SkipRegex(regex, onFailure))
        case _                                    =>
          CaptureString(inner)
      }
    }

    override protected def stripNode(state: OptimizerState): Parser[Err2, Char, String] =
      CaptureString(parser.stripNode(state))

    override protected def parseRec(state: ParserState[Char]): String = {
      val discard       = state.discard
      val startPosition = state.position
      state.discard = true
      val _             = parser.parseRec(state)
      state.discard = discard
      if (!discard && state.error == null) {
        val endPosition = state.position
        state.sliceToString(startPosition, endPosition)
      } else {
        null
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by `zip`. */
  final case class Zip[Err, Err2, In, In2 <: In, Result, Result2, ZippedResult](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Result2],
      zip: (Result, Result2) => ZippedResult
  ) extends Parser[Err2, In2, ZippedResult] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, ZippedResult] = Zip(left.runOptimizeNode(state), right.runOptimizeNode(state), zip)

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, ZippedResult] = Zip(left.runStripNode(state), right.runOptimizeNode(state), zip)

    override protected def parseRec(state: ParserState[In2]): ZippedResult = {
      val l = left.parseRec(state)
      if (state.error == null) {
        val r = right.parseRec(state)
        if (!state.discard && state.error == null) {
          zip(l, r)
        } else {
          null.asInstanceOf[ZippedResult]
        }
      } else {
        null.asInstanceOf[ZippedResult]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  /** The parser built by `zipLeft`. */
  final case class ZipLeft[Err, Err2, In, In2 <: In, Result](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Any]
  ) extends Parser[Err2, In2, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result] = ZipLeft(left.runOptimizeNode(state), right.runOptimizeNode(state))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result] = ZipLeft(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: ParserState[In2]): Result = {
      val l = left.parseRec(state)
      if (state.error == null) {
        val discard = state.discard
        state.discard = true
        val _       = right.parseRec(state)
        state.discard = discard
        if (state.error == null) {
          l
        } else {
          null.asInstanceOf[Result]
        }
      } else {
        null.asInstanceOf[Result]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  /** The parser built by `zipRight`. */
  final case class ZipRight[Err, Err2, In, In2 <: In, Result, Result2](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = ZipRight(left.runOptimizeNode(state), right.runOptimizeNode(state))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = ZipRight(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: ParserState[In2]): Result2 = {
      val discard = state.discard
      state.discard = true
      val _       = left.parseRec(state)
      state.discard = discard
      if (state.error == null) {
        val r = right.parseRec(state)
        if (state.error == null) {
          r
        } else {
          null.asInstanceOf[Result2]
        }
      } else {
        null.asInstanceOf[Result2]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  /** The parser built by `flatMap`. */
  final case class FlatMap[Err, Err2, In, In2 <: In, Result, Result2](
      parser: Parser[Err, In, Result],
      f: Result => Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = FlatMap(parser.runOptimizeNode(state), f.andThen(_.optimized))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = FlatMap(parser.runStripNode(state), f)

    override protected def parseRec(state: ParserState[In2]): Result2 = {
      val discard = state.discard
      state.discard = false
      val value   = parser.parseRec(state)
      state.discard = discard
      if (state.error == null) {
        val next = f(value)
        next.parseRec(state)
      } else {
        null.asInstanceOf[Result2]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  /** The parser built by `orElseEither`. */
  final case class OrElseEither[Err, Err2, In, In2 <: In, Result, Result2](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Either[Result, Result2]] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Either[Result, Result2]] =
      OrElseEither(
        (if (state.autoBacktrack)
           Backtrack(left)
         else
           left).runOptimizeNode(state),
        right.runOptimizeNode(state)
      )

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Either[Result, Result2]] =
      OrElseEither(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: ParserState[In2]): Either[Result, Result2] = {
      val startPosition = state.position
      val leftResult    = left.parseRec(state)
      if (state.error == null) {
        if (!state.discard)
          Left(leftResult)
        else
          null
      } else {
        if (state.position == startPosition) {
          val leftFailure = state.error
          state.error = null

          val rightResult = right.parseRec(state)
          if (state.error == null) {
            if (!state.discard)
              Right(rightResult)
            else
              null
          } else {
            state.error = leftFailure.addFailedBranch(state.error)
            null.asInstanceOf[Either[Result, Result2]]
          }
        } else {
          null.asInstanceOf[Either[Result, Result2]]
        }
      }
    }

    override protected lazy val needsBacktrack: Boolean = left.needsBacktrack || right.needsBacktrack
  }

  /** The parser built by `orElse`. */
  final case class OrElse[Err, Err2, In, In2 <: In, Result, Result2](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = {
      val optimizedLeft  =
        (if (state.autoBacktrack)
           Backtrack(left)
         else
           left).runOptimizeNode(state)
      val optimizedRight = right.runOptimizeNode(state)

      (optimizedLeft, optimizedRight) match {
        case (captureLeft: CaptureString[_, _], captureRight: CaptureString[_, _]) =>
          (captureLeft.parser, captureRight.parser) match {
            case (SkipRegex(regexL, onFailureL), SkipRegex(regexR, onFailureR)) =>
              CaptureString(
                SkipRegex(
                  regexL | regexR,
                  onFailureR.orElse(onFailureL): @nowarn
                )
              ).asInstanceOf[Parser[Err2, In2, Result2]]
            case (_, _)                                                         =>
              OrElse(
                optimizedLeft,
                optimizedRight
              )
          }
        case (_, _)                                                                =>
          OrElse(
            optimizedLeft,
            optimizedRight
          )
      }
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = OrElse(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: ParserState[In2]): Result2 = {
      val startPosition = state.position
      val leftResult    = left.parseRec(state)
      if (state.error == null) {
        leftResult.asInstanceOf[Result2]
      } else {
        if (state.position == startPosition) {
          val leftFailure = state.error
          state.error = null

          val rightResult = right.parseRec(state)
          if (state.error == null) {
            rightResult
          } else {
            state.error = leftFailure.addFailedBranch(state.error)
            null.asInstanceOf[Result2]
          }
        } else {
          null.asInstanceOf[Result2]
        }
      }
    }

    override protected lazy val needsBacktrack: Boolean = left.needsBacktrack || right.needsBacktrack
  }

  /** The parser built by `optional`. */
  final case class Optional[Err, In, Result](parser: Parser[Err, In, Result]) extends Parser[Err, In, Option[Result]] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Option[Result]] =
      Optional(
        (if (state.autoBacktrack)
           Backtrack(parser)
         else
           parser).runOptimizeNode(state)
      )

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Option[Result]] = Optional(parser.runStripNode(state))

    override protected def parseRec(state: ParserState[In]): Option[Result] = {
      val startPos = state.position
      val result   = parser.parseRec(state)
      if (state.error == null) {
        if (!state.discard)
          Some(result)
        else null
      } else {
        if (state.position != startPos) {
          null.asInstanceOf[Option[Result]]
        } else {
          state.error = null
          None
        }
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by the repetition combinators; `max = None` means no upper limit. */
  final case class Repeat[Err, In, Result](parser: Parser[Err, In, Result], min: Int, max: Option[Int])
      extends Parser[Err, In, Chunk[Result]] {

    /** The initial size of the result builder: `max` if given, otherwise `min * 2` but at least 128. */
    val hint: Int             = max.getOrElse(Math.max(min * 2, 128))
    private val maxCount: Int = max.getOrElse(Int.MaxValue)

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Chunk[Result]] = {
      val optimizedInner = (if (state.autoBacktrack)
                              Backtrack(parser)
                            else
                              parser).runOptimizeNode(state)

      optimizedInner match {
        case parseRegexLc: ParseRegexLastChar[_] =>
          max match {
            case Some(max) =>
              ParseRegex(parseRegexLc.regex.between(min, max), parseRegexLc.onFailure)
                .asInstanceOf[Parser[Err, In, Chunk[Result]]]
            case None      =>
              ParseRegex(parseRegexLc.regex.atLeast(min), parseRegexLc.onFailure)
                .asInstanceOf[Parser[Err, In, Chunk[Result]]]
          }
        case _                                   =>
          Repeat(optimizedInner, min, max)
      }
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Chunk[Result]] = {
      val optimizedInner = parser.runStripNode(state)
      Repeat(optimizedInner, min, max)
    }

    override protected def parseRec(state: ParserState[In]): Chunk[Result] = {
      val discard       = state.discard
      val builder       = if (discard) null else ChunkBuilder.make[Result](hint)
      var count         = 0
      var lastItemStart = -1
      val sourceLength  = state.length

      while (state.error == null && count < maxCount && lastItemStart < sourceLength) {
        lastItemStart = state.position

        val item = parser.parseRec(state)
        if (state.error == null) {
          count = count + 1
          if (!discard)
            builder += item
        }
      }

      if (count < min && state.error == null) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
      } else {
        if (count >= min) {
          state.error = null
        }
      }

      if (!discard && state.error == null) {
        builder.result()
      } else {
        null.asInstanceOf[Chunk[Result]]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  /** The parser built by `not`. */
  final case class Not[Err, In](parser: Parser[Err, In, Any], failure: Err) extends Parser[Err, In, Unit] {
    override protected def optimizeNode(state: OptimizerState): Parser[Err, In, Unit] =
      Not(parser.runOptimizeNode(state), failure)

    override protected def stripNode(state: OptimizerState): Parser[Err, In, Unit] =
      Not(parser.runStripNode(state), failure)

    override protected def parseRec(state: ParserState[In]): Unit = {
      val discard = state.discard
      state.discard = true
      val _       = parser.parseRec(state)
      state.discard = discard
      if (state.error == null) {
        state.error = ParserError.Failure(state.nameStack, state.position, failure)
      } else {
        state.error = null
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser built by `backtrack`. */
  final case class Backtrack[Err, In, Result](parser: Parser[Err, In, Result]) extends Parser[Err, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = {
      val inner = parser.runOptimizeNode(state)
      if (inner.needsBacktrack)
        Backtrack(inner)
      else inner
    }

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = Backtrack(parser.runStripNode(state))

    override protected def parseRec(state: ParserState[In]): Result = {
      val position = state.position
      val result   = parser.parseRec(state)
      if (state.error != null) {
        state.position = position
      }
      result
    }

    override protected val needsBacktrack: Boolean = false
  }

  /** The parser built by `setAutoBacktracking`. The optimizer removes it. */
  final case class SetAutoBacktrack[Err, In, Result](parser: Parser[Err, In, Result], enabled: Boolean)
      extends Parser[Err, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] =
      parser.runOptimizeNode(state.copy(autoBacktrack = enabled))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = SetAutoBacktrack(parser.runStripNode(state), enabled)

    override protected def parseRec(state: ParserState[In]): Result =
      ??? // Optimize always removes this node

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** A parser that maps the [[Parser.ParserError]] of the inner parser; `mapError` builds it. */
  final case class MapError[Err, Err2, In, Result](
      parser: Parser[Err, In, Result],
      mapParserErr: ParserError[Err] => ParserError[Err2]
  ) extends Parser[Err2, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In, Result] = MapError(parser.runOptimizeNode(state), mapParserErr)

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In, Result] = MapError(parser.runStripNode(state), mapParserErr)

    override protected def parseRec(state: ParserState[In]): Result = {
      val result = parser.parseRec(state)
      if (state.error != null)
        state.error = mapParserErr(state.error.asInstanceOf[ParserError[Err]])
      result
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  /** The parser behind `Parser.index`. */
  case object Index extends Parser[Nothing, Any, Int] {
    override protected def optimizeNode(state: OptimizerState): Parser[Nothing, Any, Int] = this
    override protected def stripNode(state: OptimizerState): Parser[Nothing, Any, Int]    = this

    override protected def parseRec(state: ParserState[Any]): Int = state.position

    override protected val needsBacktrack: Boolean = false
  }

  /** The parser behind `Parser.end`. */
  case object End extends Parser[Nothing, Any, Unit] {
    override protected def optimizeNode(state: OptimizerState): Parser[Nothing, Any, Unit] = this
    override protected def stripNode(state: OptimizerState): Parser[Nothing, Any, Unit]    = this

    override protected def parseRec(state: ParserState[Any]): Unit =
      if (state.position < state.length)
        state.error = ParserError.NotConsumedAll(state.nameStack, state.position)

    override protected def needsBacktrack: Boolean = false
  }

  /** Parser that does not consume any input and succeeds with 'value'
    *
    * {{{
    * Parser.succeed(42).parseString("") // Right(42)
    * }}}
    */
  def succeed[Result](value: Result): Parser[Nothing, Any, Result] = Succeed(value)

  /** Parser that always fails with 'failure'
    *
    * {{{
    * Parser.fail("unsupported").parseString("x") // Left(StringParserError(x,Failure(List(),0,unsupported)))
    * }}}
    */
  def fail[Err](failure: Err): Parser[Err, Any, Nothing] = Fail(failure).asInstanceOf[Parser[Err, Any, Nothing]]

  // Char variants
  /** Parser that consumes the exact character 'value' or fails if it did not match, and results in the unit value. The
    * failure is the string "not 'c'" for the character c.
    *
    * {{{
    * Parser.char('{').parseString("[") // Left(StringParserError([,Failure(List(),0,not '{')))
    * }}}
    */
  def char(value: Char): Parser[String, Char, Unit] =
    char(value, s"not '$value'")

  /** Parser that consumes the exact character 'value' or fails with 'failure' if it did not match, and results in the
    * unit value
    */
  def char[Err](value: Char, failure: Err): Parser[Err, Char, Unit] =
    regexDiscard(Regex.charIn(value), failure)

  /** Parser that consumes a single character, fails if it is 'value', otherwise results with it. The failure is the
    * string "cannot be 'c'" for the character c.
    */
  def notChar(value: Char): Parser[String, Char, Char] =
    notChar(value, s"cannot be '$value'")

  /** Parser that consumes a single character, fails with 'failure' if it is 'value', otherwise results with it */
  def notChar[Err](value: Char, failure: Err): Parser[Err, Char, Char] =
    regexChar(Regex.charNotIn(value), failure)

  /** Parser that executes a regular expression on the input but discards its result, and fails with 'failure' if the
    * regex fails. When the regex needs more input than there is, the error is
    * [[Parser.ParserError.UnexpectedEndOfInput]]; currently that is also the error when a repetition in the regex
    * matches fewer times than its minimum.
    */
  def regexDiscard[Err](regex: Regex, failure: Err): Parser[Err, Char, Unit] =
    Parser.SkipRegex(regex, Some(failure))

  /** Parser that executes a regular expression on the input but discards its result. The regex is supposed to never
    * fail. If it does not match, the default implementation fails with [[Parser.ParserError.UnknownFailure]], or with
    * [[Parser.ParserError.UnexpectedEndOfInput]] in the cases described for `regexDiscard`.
    */
  def unsafeRegexDiscard(regex: Regex): Parser[Nothing, Char, Unit] =
    Parser.SkipRegex(regex, None)

  /** Parser that executes a regular expression on the input and results in the last parsed character, or fails with
    * 'failure'. Useful for regexes that are known to parse a single character.
    *
    * {{{
    * Parser.regexChar(Regex.anyDigit, "digit").parseString("5") // Right(5)
    * }}}
    */
  def regexChar[Err](regex: Regex, failure: Err): Parser.ParseRegexLastChar[Err] =
    Parser.ParseRegexLastChar(regex, Some(failure))

  /** Parser that executes a regular expression on the input and results in the last parsed character. The regex is
    * supposed to never fail. Useful for regexes that are known to parse a single character. If it does not match, the
    * default implementation fails with [[Parser.ParserError.UnknownFailure]], or with
    * [[Parser.ParserError.UnexpectedEndOfInput]] in the cases described for `regexDiscard`.
    */
  def unsafeRegexChar[E](regex: Regex): Parser.ParseRegexLastChar[E] =
    Parser.ParseRegexLastChar(regex, None)

  /** Parser that executes a regular expression on the input and results in the chunk of the parsed characters, or fails
    * with 'failure'. As with `regexDiscard`, a regex that needs more input fails with
    * [[Parser.ParserError.UnexpectedEndOfInput]] instead.
    *
    * {{{
    * Parser.regex(Regex.digits, "no digits").map(_.mkString).parseString("2024!") // Right(2024)
    * }}}
    */
  def regex[Err](regex: Regex, failure: Err): Parser.ParseRegex[Err] =
    Parser.ParseRegex(regex, Some(failure))

  /** Parser that executes a regular expression on the input and results in the chunk of the parsed characters. The
    * regex is supposed to never fail. If it does not match, the default implementation fails with
    * [[Parser.ParserError.UnknownFailure]], or with [[Parser.ParserError.UnexpectedEndOfInput]] in the cases described
    * for `regexDiscard`.
    */
  def unsafeRegex(regex: Regex): Parser.ParseRegex[Nothing] =
    Parser.ParseRegex(regex, None)

  /** Parser that consumes a single element of any type and results in it. It fails with
    * [[Parser.ParserError.UnexpectedEndOfInput]] at the end of the input. Use it with `parseChunk` for non-character
    * input; currently [[ParserImplementation.StackSafe]] does not support it and throws a `scala.NotImplementedError`.
    */
  def any[T]: Parser[Nothing, T, T] = Parser.Passthrough[T, T]()

  /** Parser that consumes a single character and returns it. It fails with [[Parser.ParserError.UnexpectedEndOfInput]]
    * at the end of the input.
    */
  val anyChar: Parser[Nothing, Char, Char] =
    unsafeRegexChar(Regex.anyChar)

  /** Parser that consumes a single character and succeeds with it if it is one of the provided 'chars'
    *
    * {{{
    * Parser.charIn('+', '-').parseString("*")
    * // Left(StringParserError(*,Failure(List(),0,Not the expected character (+, -))))
    * }}}
    */
  def charIn(chars: Char*): Parser[String, Char, Char] =
    regexChar(Regex.charIn(chars: _*), s"Not the expected character (${chars.mkString(", ")})")

  /** Parser that consumes a single character and succeeds with it if it is NOT one of the provided 'chars' */
  def charNotIn(chars: Char*): Parser[String, Char, Char] =
    regexChar(Regex.charNotIn(chars: _*), s"One of the excluded characters (${chars.mkString(", ")})")

  /** Parser that consumes and discards all the remaining input */
  lazy val ignoreRest: Parser[Nothing, Char, Unit] =
    unsafeRegexDiscard(Regex.anyChar.atLeast(0))

  /** Parser that consumes the rest of the input and captures it as a string. It succeeds with an empty string at the
    * end of the input.
    */
  val anyString: Parser[Nothing, Char, String] =
    unsafeRegexDiscard(Regex.anyChar.atLeast(0)).string

  /** Parser that requires a given string 'str' and in case it could read it, results in 'value'. The failure is the
    * string "Not 'str'"; input that ends inside 'str' fails with [[Parser.ParserError.UnexpectedEndOfInput]].
    *
    * {{{
    * Parser.string("true", true).parseString("tree") // Left(StringParserError(tree,Failure(List(),0,Not 'true')))
    * }}}
    */
  def string[Result](str: String, value: Result): Parser[String, Char, Result] =
    regexDiscard(Regex.string(str), s"Not '$str'").as(value)

  /** Parser that does not consume the input and results in unit */
  lazy val unit: Parser[Nothing, Any, Unit] = succeed(())

  /** Parser of a single alpha-numeric character (`Char.isLetter` or `Char.isDigit`) */
  lazy val alphaNumeric: Parser[String, Char, Char] = regexChar(Regex.anyAlphaNumeric, "not alphanumeric")

  /** Parser of a single digit (`Char.isDigit`, so other Unicode digits too)
    *
    * {{{
    * Parser.digit.parseString("x") // Left(StringParserError(x,Failure(List(),0,not a digit)))
    * }}}
    */
  lazy val digit: Parser[String, Char, Char] = regexChar(Regex.anyDigit, "not a digit")

  /** Parser of a single letter (`Char.isLetter`) */
  lazy val letter: Parser[String, Char, Char] = regexChar(Regex.anyLetter, "not a letter")

  /** Parser built from `Regex.whitespace`, which matches zero or more whitespace characters, resulting in the last
    * character of the match.
    *
    * Currently, when no whitespace follows, it results in the character before the current position, or throws a
    * `java.lang.StringIndexOutOfBoundsException` at the start of the input. `Syntax.whitespace` matches exactly one
    * whitespace character.
    */
  lazy val whitespace: Parser[String, Char, Char] = regexChar(Regex.whitespace, "not a whitespace")

  /** Parser that results in the current input stream position
    *
    * {{{
    * (Parser.letter.repeat ~> Parser.index).parseString("abc1") // Right(3)
    * }}}
    */
  lazy val index: Parser[Nothing, Any, Int] = Parser.Index

  /** Parser that only succeeds if the input stream has been consumed fully.
    *
    * This can be used to require that a parser consumes the full input. Otherwise it fails with
    * [[Parser.ParserError.NotConsumedAll]].
    *
    * {{{
    * (Parser.digit <~ Parser.end).parseString("12") // Left(StringParserError(12,NotConsumedAll(List(),1)))
    * }}}
    */
  lazy val end: Parser[Nothing, Any, Unit] = Parser.End

  /** Type representing a parser error. `parseChunk` returns it directly, and `parseString` and `parseChars` return it
    * inside a [[StringParserError]].
    *
    * {{{
    * val pair = Parser.letter ~ Parser.digit
    * pair.parseString("ab") match {
    *   case Left(StringParserError(_, Parser.ParserError.Failure(_, position, failure))) => failure + " at " + position
    *   case Left(other)                                                               => other.error.toString
    *   case Right(value)                                                              => value.toString
    * }
    * // "not a digit at 1"
    * }}}
    *
    * @see
    *   [[https://zio.dev/zio-parser/errors/ Error Handling]]
    */
  sealed trait ParserError[+Err] { self =>

    /** Returns `AllBranchesFailed(this, error)`: this error combined with the error of an alternative that was tried
      * after it.
      */
    def addFailedBranch[Err2 >: Err](error: ParserError[Err2]): ParserError[Err2] =
      ParserError.AllBranchesFailed(self, error)

    /** Maps the custom error value of every [[Parser.ParserError.Failure]] in this error, including the ones inside
      * [[Parser.ParserError.AllBranchesFailed]]. The other cases are kept as they are.
      *
      * {{{
      * Parser.ParserError.Failure(List("year"), 3, "boom").map(_.length) // Failure(List(year),3,4)
      * }}}
      */
    def map[Err2](f: Err => Err2): ParserError[Err2] = self match {
      case ParserError.Failure(nameStack, position, failure) => ParserError.Failure(nameStack, position, f(failure))
      case ParserError.UnknownFailure(nameStack, position)   => ParserError.UnknownFailure(nameStack, position)
      case ParserError.UnexpectedEndOfInput(nameStack)       => ParserError.UnexpectedEndOfInput(nameStack)
      case ParserError.NotConsumedAll(nameStack, position)   => ParserError.NotConsumedAll(nameStack, position)
      case ParserError.AllBranchesFailed(left, right)        => ParserError.AllBranchesFailed(left.map(f), right.map(f))
    }
  }

  /** The cases of [[Parser.ParserError]]. */
  object ParserError {

    /** User-defined parser error of type 'Err'
      *
      * {{{
      * Parser.char(':', "colon expected").parseString("=")
      * // Left(StringParserError(=,Failure(List(),0,colon expected)))
      * }}}
      *
      * @param nameStack
      *   Stack of named parsers until reaching the failure
      * @param position
      *   Input stream position
      * @param failure
      *   The custom failure
      * @tparam Err
      *   Failure type
      */
    final case class Failure[Err](nameStack: List[String], position: Int, failure: Err) extends ParserError[Err]

    /** Unknown parser error, without a custom failure value. The unsafe regex parsers (`Parser.unsafeRegex`,
      * `Parser.unsafeRegexChar`, `Parser.unsafeRegexDiscard`) produce it when their regex does not match, with the
      * default implementation.
      *
      * {{{
      * Parser.unsafeRegex(Regex.string("GET")).parseString("PUT")
      * // Left(StringParserError(PUT,UnknownFailure(List(),0)))
      * }}}
      *
      * @param nameStack
      *   Stack of named parsers until reaching the failure
      * @param position
      *   Input stream position
      */
    final case class UnknownFailure(nameStack: List[String], position: Int) extends ParserError[Nothing]

    /** The input stream ended before the parser finished. Regex-based parsers also report it when a repetition in the
      * regex matched fewer times than its minimum, and repeated parsers can report it when they read fewer items than
      * required.
      *
      * {{{
      * Parser.string("true", true).parseString("tr") // Left(StringParserError(tr,UnexpectedEndOfInput(List())))
      * }}}
      */
    final case class UnexpectedEndOfInput(nameStack: List[String]) extends ParserError[Nothing]

    /** The parser was supposed to consume the full input but it did not. `Parser.end` produces it.
      *
      * {{{
      * (Parser.digit <~ Parser.end).parseString("12") // Left(StringParserError(12,NotConsumedAll(List(),1)))
      * }}}
      *
      * @param nameStack
      *   Stack of named parsers until reaching the failure
      * @param position
      *   Input stream position of the first input that was not consumed
      */
    final case class NotConsumedAll[Err](nameStack: List[String], position: Int) extends ParserError[Err]

    /** All branches failed in a sequence of orElse or orElseEither parsers.
      *
      * Every failed branch's failure is preserved.
      *
      * The error of `a | b | c` is `AllBranchesFailed(AllBranchesFailed(a, b), c)`.
      *
      * {{{
      * (Parser.char('a') | Parser.char('b')).parseString("c")
      * // Left(StringParserError(c,AllBranchesFailed(Failure(List(),0,not 'a'),Failure(List(),0,not 'b'))))
      * }}}
      */
    final case class AllBranchesFailed[Err](left: ParserError[Err], right: ParserError[Err]) extends ParserError[Err]
  }
}

/** The error of parsing a string or a chunk of characters: the [[Parser.ParserError]] together with the 'input'.
  * `pretty` renders it for humans.
  */
final case class StringParserError[+Err](input: String, error: ParserError[Err]) {

  /** Pretty prints the error with the input context. For a [[Parser.ParserError.Failure]],
    * [[Parser.ParserError.UnknownFailure]] or [[Parser.ParserError.NotConsumedAll]] it shows up to two lines of input
    * before and after the failing line, a caret under the failing position and an `error:` line; other errors are
    * printed as a single line, such as "Unexpected end of input".
    */
  def pretty: String = {
    val sb = new StringBuilder

    def errorBuilder(error: ParserError[Err]): Unit =
      error match {
        case ParserError.Failure(nameStack, position, failure) =>
          sb.append(s"Failure at position $position: $failure")
          if (nameStack.nonEmpty) {
            sb.append(" in ")
            sb.append(nameStack.mkString(" -> "))
          }
        case ParserError.UnknownFailure(nameStack, position)   =>
          sb.append(s"Unknown failure at position $position")
          if (nameStack.nonEmpty) {
            sb.append(" in ")
            sb.append(nameStack.mkString(" -> "))
          }
        case ParserError.UnexpectedEndOfInput(nameStack)       =>
          sb.append("Unexpected end of input")
          if (nameStack.nonEmpty) {
            sb.append(" in ")
            sb.append(nameStack.mkString(" -> "))
          }
        case ParserError.NotConsumedAll(nameStack, position)   =>
          sb.append(s"Parser did not consume all input at position $position")
          if (nameStack.nonEmpty) {
            sb.append(" in ")
            sb.append(nameStack.mkString(" -> "))
          }
        case ParserError.AllBranchesFailed(left, right)        =>
          sb.append("All branches failed: ")
          errorBuilder(left)
          sb.append(" and ")
          errorBuilder(right)
      }

    def printError(pos: Int) = {
      val lines          = input.split("\n")
      val positions      = lines.scanLeft(0)(_ + _.length + 1) // +1 for newline character
      val errorLineIndex = positions.indexWhere(_ > pos) - 1

      val contextRange = (math.max(errorLineIndex - 2, 0), math.min(errorLineIndex + 2, lines.length - 1))

      if (contextRange._1 > 0) sb.append("...\n")

      lines.slice(contextRange._1, contextRange._2 + 1).zipWithIndex.foreach { case (line, index) =>
        val actualIndex = contextRange._1 + index
        sb.append(line).append("\n")
        if (actualIndex == errorLineIndex) {
          val columnPos = pos - positions(errorLineIndex)
          sb.append(" " * columnPos)
          sb.append("^\n")
          sb.append("error: ")
          errorBuilder(error)
          sb.append("\n")
        }
      }

      if (contextRange._2 < lines.length - 1) sb.append("...\n")
    }

    error match {
      case ParserError.Failure(_, position, _)     => printError(position)
      case ParserError.UnknownFailure(_, position) => printError(position)
      case ParserError.NotConsumedAll(_, position) => printError(position)
      case _                                       => errorBuilder(error)
    }

    sb.toString()
  }
}
