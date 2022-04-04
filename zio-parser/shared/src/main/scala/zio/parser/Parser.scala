package zio.parser

import zio.parser.Parser.{ErasedParser, ParserError}
import zio.parser.internal.stacksafe.ParserOp.InitialParser
import zio.parser.internal.stacksafe.{CharParserImpl, ParserOp}
import zio.parser.internal.{PZippable, recursive}
import zio.{Chunk, ChunkBuilder}

import scala.annotation.nowarn
import scala.collection.mutable

/** A Parser consumes a stream of 'In's and either fails with a ParserError possibly holding a custom error of type
  * 'Err' or succeeds with a result of type `Result`
  *
  * Parsers can be combined with Printers to get Syntax, or a Parser and a Printer can be built simultaneously by using
  * the combinators of Syntax.
  *
  * Recursive parsers can be expressed directly by recursing in one of the zipping or or-else combinators.
  *
  * By default a parser backtracks automatically. This behavior can be changed with the 'manualBacktracking' operator.
  *
  * Parsers trees get optimized automatically before running the parser. This optimized tree can be examined by the
  * 'optimized' field. For the full list of transformations performed in the optimization phase check each parser node's
  * 'optimizeNode' method.
  *
  * @tparam Err
  *   Custom error type
  * @tparam In
  *   Element type of the input stream of parsing
  * @tparam Result
  *   The type of the parsed result value
  */
sealed trait Parser[+Err, -In, +Result] { self =>

  /** Maps the parser's successful result with the given function 'to' */
  final def map[Result2](to: Result => Result2): Parser[Err, In, Result2] =
    Parser.Transform(self, to)

  /** Ignores the parser's successful result and result in 'result' instead */
  final def as[Result2](result: Result2): Parser[Err, In, Result2] =
    Parser.Ignore(self, result)

  /** Maps the parser's successful result with the given function 'to' that either fails or produces a new result value.
    */
  final def transformEither[Err2, Result2](
      to: Result => Either[Err2, Result2]
  ): Parser[Err2, In, Result2] =
    Parser.TransformEither(self, to)

  /** Maps the parser's successful result with the given function 'to' that either produces a new result value or the
    * failure is indicated in the error channel by the value None.
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

  /** Concatenates this parser with 'that' parser. In case both parser succeeds, the result is a pair of the results. */
  final def zip[Err2 >: Err, In2 <: In, Result2, ZippedResult](
      that: => Parser[Err2, In2, Result2]
  )(implicit zippable: PZippable.Out[Result, Result2, ZippedResult]): Parser[Err2, In2, ZippedResult] =
    Parser.Zip(Parser.Lazy(() => self), Parser.Lazy(() => that), zippable.zip)

  /** Symbolic alias for zipLeft */
  final def <~[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Unit]
  ): Parser[Err2, In2, Result] =
    zipLeft(that)

  /** Concatenates this parser with 'that' parser. In case both parser succeeds, the result is the result of this
    * parser. Otherwise the parser fails.
    */
  final def zipLeft[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Any]
  ): Parser[Err2, In2, Result] =
    Parser.ZipLeft(Parser.Lazy(() => self), Parser.Lazy(() => that))

  /** Determines the continuation of the parser by the result of this parser, expressed by the function 'that' */
  final def flatMap[Err2 >: Err, In2 <: In, Result2](
      that: Result => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result2] =
    Parser.FlatMap(self, that)

  /** Checks the result of this parser with the given function. If the 'condition' is false, fails with the given
    * failure 'failure', otherwise results in the this parser's result.
    */
  final def filter[Result2, Err2 >: Err](condition: Result2 => Boolean, failure: Err2)(implicit
      ev: Result <:< Result2
  ): Parser[Err2, In, Result2] =
    transformEither((d2: Result) => if (condition(ev(d2))) Right(d2) else Left(failure))

  /** Associates a name with this parser. The chain of named parsers are reported in case of failure to help debugging
    * parser issues.
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
    * this parser's result. In case it fails, the result is 'that' parser's result.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
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
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
    */
  final def orElseEither[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Either[Result, Result2]] =
    Parser.OrElseEither(Parser.Lazy(() => self), Parser.Lazy(() => that))

  /** Repeats this parser at least 'min' times.
    *
    * The result is all the parsed elements until the first failure. The failure that stops the repetition gets
    * swallowed and in case auto-backtracking is on, the parser backtracks to the end of the last successful item.
    */
  final def atLeast(min: Int): Parser[Err, In, Chunk[Result]] =
    Parser.Repeat(self, min, None)

  /** Repeats this parser at least once.
    *
    * The result is all the parsed elements until the first failure. The failure that stops the repetition gets
    * swallowed and in case auto-backtracking is on, the parser backtracks to the end of the last successful item.
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

  /** Repeats this parser at least once and requires that between each element, the 'sep' parser succeeds */
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

  /** Repeats this parser until the given `stopCondition` parser succeeds. */
  final def repeatUntil[Err2 >: Err, In2 <: In](
      stopCondition: Parser[Err2, In2, Unit]
  ): Parser[Err2, In2, Chunk[Result]] =
    (stopCondition.not(null.asInstanceOf[Err2]) ~> self).repeat0.manualBacktracking

  /** Make this parser optional.
    *
    * Failure of this parser will be ignored. In case auto-backtracking is enabled, backtracking is performed on it.
    */
  final def optional: Parser[Err, In, Option[Result]] =
    Parser.Optional(self)

  /** Symbolic alias for optional */
  final def ? : Parser[Err, In, Option[Result]] = optional

  /** Concatenates the parsers 'left', then this, then 'right'.
    *
    * All three must succeed. The result is this parser's result.
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

  /** Maps the error with the given function 'f' */
  final def mapError[Err2](f: Err => Err2): Parser[Err2, In, Result] =
    Parser.MapError[Err, Err2, In, Result](
      self,
      _.map(f)
    )

  /** Ignores this parser's result and instead capture the parsed string fragment */
  final def string(implicit ev: Char <:< In): Parser[Err, Char, String] =
    Parser.CaptureString[Err, Err](self.asInstanceOf[Parser[Err, Char, Result]])

  /** Flattens a result of parsed strings to a single string */
  final def flatten(implicit
      ev2: Result <:< Chunk[String]
  ): Parser[Err, In, String] =
    map(s => s.mkString)

  /** Parser that does not consume any input and produces the unit value */
  final def unit: Parser[Err, In, Unit] = as(())

  /** Parser that fails with the given 'failure' if this parser succeeds */
  final def not[Err2 >: Err](failure: => Err2): Parser[Err2, In, Unit] =
    Parser.Not(self, failure)

  /** Parser that resets the parsing position in case it fails.
    *
    * By default backtracking points are automatically inserted. This behavior can be changed with the autoBacktracking,
    * manualBacktracking and setAutoBacktracking combinators.
    */
  final def backtrack: Parser[Err, In, Result] = Parser.Backtrack(self)

  /** Enables auto-backtracking for this parser */
  final def autoBacktracking: Parser[Err, In, Result] =
    setAutoBacktracking(true)

  /** Turns off auto-backtracking for this parser */
  final def manualBacktracking: Parser[Err, In, Result] =
    setAutoBacktracking(false)

  /** Enables or disables auto-backtracking for this parser */
  final def setAutoBacktracking(enabled: Boolean): Parser[Err, In, Result] =
    Parser.SetAutoBacktrack(self, enabled)

  // Execution
  /** Run this parser on the given 'input' string */
  final def parseString(input: String)(implicit ev: Char <:< In): Either[ParserError[Err], Result] =
    parseString(input, self.defaultImplementation)

  /** Run this parser on the given 'input' string using a specific parser implementation */
  final def parseString(input: String, parserImplementation: ParserImplementation)(implicit
      ev: Char <:< In
  ): Either[ParserError[Err], Result] =
    parserImplementation match {
      case ParserImplementation.StackSafe =>
        val parser = new CharParserImpl[Err, Result](
          self.compiledOpStack,
          input
        )
        parser.run()
      case ParserImplementation.Recursive =>
        val state  = new recursive.ParserState(input)
        val result = self.optimized.parseRec(state)

        if (state.error != null)
          Left(state.error.asInstanceOf[ParserError[Err]])
        else
          Right(result)
    }

  /** Run this parser on the given 'input' chunk of characters */
  final def parseChars(input: Chunk[Char])(implicit ev: Char <:< In): Either[ParserError[Err], Result] =
    parseChars(input, self.defaultImplementation)

  /** Run this parser on the given 'input' chunk of characters using a specific parser implementation */
  final def parseChars(input: Chunk[Char], parserImplementation: ParserImplementation)(implicit
      ev: Char <:< In
  ): Either[ParserError[Err], Result] =
    parseString(new String(input.toArray), parserImplementation)

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

  lazy val compiledOpStack: InitialParser = ParserOp.compile(optimized.asInstanceOf[ErasedParser])
  lazy val defaultImplementation: ParserImplementation =
    // NOTE: here we can analyse the parser tree to select an implementation (for example check if it has FlatMap)
    ParserImplementation.Recursive

  protected def parseRec(state: recursive.ParserState): Result

  protected def needsBacktrack: Boolean
}

object Parser {
  private[parser] type ErasedParser = Parser[_, _, _]

  private[parser] case class OptimizerState(
      optimized: mutable.Map[Parser[_, _, _], Parser[_, _, _]],
      visited: mutable.Map[Parser[_, _, _], Int],
      autoBacktrack: Boolean
  )

  final case class Lazy[+Err, -In, +Result](inner: () => Parser[Err, In, Result]) extends Parser[Err, In, Result] {

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

    override protected def parseRec(state: recursive.ParserState): Result =
      memoized.parseRec(state)

    override protected lazy val needsBacktrack: Boolean = true
  }

  final case class Succeed[+Result](value: Result) extends Parser[Nothing, Any, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Nothing, Any, Result] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Nothing, Any, Result] = this

    override protected def parseRec(state: recursive.ParserState): Result =
      value

    override protected val needsBacktrack: Boolean = false
  }

  final case class Fail[+Err](failure: Err) extends Parser[Err, Any, Nothing] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def parseRec(state: recursive.ParserState): Nothing = {
      state.error = ParserError.Failure(state.nameStack, state.position, failure)
      null.asInstanceOf[Nothing]
    }

    override protected val needsBacktrack: Boolean = false
  }

  final case class Failed[+Err](failure: ParserError[Err]) extends Parser[Err, Any, Nothing] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, Any, Nothing] = this

    override protected def parseRec(state: recursive.ParserState): Nothing = {
      state.error = failure
      null.asInstanceOf[Nothing]
    }

    override protected val needsBacktrack: Boolean = false
  }

  final case class Named[Err, In, Result](parser: Parser[Err, In, Result], name: String)
      extends Parser[Err, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = Named(parser.runOptimizeNode(state), name)

    override protected def stripNode(state: OptimizerState): Parser[Err, In, Result] =
      parser.runStripNode(state)

    override protected def parseRec(state: recursive.ParserState): Result = {
      state.pushName(name)
      val result = parser.parseRec(state)
      state.popName()
      result
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  final case class SkipRegex[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Unit] {

    lazy val compiledRegex: Regex.Compiled = regex.compile

    override protected def optimizeNode(state: OptimizerState): Parser[Err, Char, Unit] = {
      this.compiledRegex.test(0, "")
      this
    }

    override protected def stripNode(state: OptimizerState): Parser[Err, Char, Unit] = this

    override protected def parseRec(state: recursive.ParserState): Unit = {
      val position = state.position
      val result   = compiledRegex.test(position, state.source)
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

  final case class ParseRegex[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Chunk[Char]] {

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

    override protected def parseRec(state: recursive.ParserState): Chunk[Char] = {
      val position = state.position
      val result   = compiledRegex.test(position, state.source)
      if (result == Regex.NeedMoreInput) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
        null.asInstanceOf[Chunk[Char]]
      } else if (result == Regex.NotMatched) {
        state.error = getFailure(position, state.nameStack)
        null.asInstanceOf[Chunk[Char]]
      } else {
        state.position = result
        if (!state.discard) {
          Chunk.fromArray(state.source.slice(position, result).toCharArray)
        } else null
      }
    }

    override protected val needsBacktrack: Boolean = false

    override def toString: String = "ParseRegex()"
  }

  final case class ParseRegexLastChar[Err](regex: Regex, onFailure: Option[Err]) extends Parser[Err, Char, Char] {

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

    override protected def parseRec(state: recursive.ParserState): Char = {
      val position = state.position
      val result   = compiledRegex.test(position, state.source)
      if (result == Regex.NeedMoreInput) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
        null.asInstanceOf[Char]
      } else if (result == Regex.NotMatched) {
        state.error = getFailure(position, state.nameStack)
        null.asInstanceOf[Char]
      } else {
        state.position = result
        if (!state.discard) {
          state.source(result - 1)
        } else null.asInstanceOf[Char]
      }
    }

    override protected val needsBacktrack: Boolean = false

    override def toString: String = "ParseRegexLastChar()"
  }

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

    override protected def parseRec(state: recursive.ParserState): Result2 = {
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

    override protected def parseRec(state: recursive.ParserState): Result2 = {
      val result = parser.parseRec(state)
      if (!state.discard && state.error == null) {
        to(result)
      } else {
        null.asInstanceOf[Result2]
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

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

    override protected def parseRec(state: recursive.ParserState): Result2 = {
      val discard = state.discard
      state.discard = true
      val _       = parser.parseRec(state)
      state.discard = discard
      to
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

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

    override protected def parseRec(state: recursive.ParserState): String = {
      val discard       = state.discard
      val startPosition = state.position
      state.discard = true
      val _             = parser.parseRec(state)
      state.discard = discard
      if (!discard && state.error == null) {
        val endPosition = state.position
        state.source.slice(startPosition, endPosition)
      } else {
        null
      }
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  final case class Zip[Err, Err2, In, In2, Result, Result2, ZippedResult](
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

    override protected def parseRec(state: recursive.ParserState): ZippedResult = {
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

  final case class ZipLeft[Err, Err2, In, In2, Result](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Any]
  ) extends Parser[Err2, In2, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result] = ZipLeft(left.runOptimizeNode(state), right.runOptimizeNode(state))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result] = ZipLeft(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: recursive.ParserState): Result = {
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

  final case class ZipRight[Err, Err2, In, In2, Result, Result2](
      left: Parser[Err, In, Result],
      right: Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = ZipRight(left.runOptimizeNode(state), right.runOptimizeNode(state))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = ZipRight(left.runStripNode(state), right.runStripNode(state))

    override protected def parseRec(state: recursive.ParserState): Result2 = {
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

  final case class FlatMap[Err, Err2, In, In2, Result, Result2](
      parser: Parser[Err, In, Result],
      f: Result => Parser[Err2, In2, Result2]
  ) extends Parser[Err2, In2, Result2] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = FlatMap(parser.runOptimizeNode(state), f.andThen(_.optimized))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err2, In2, Result2] = FlatMap(parser.runStripNode(state), f)

    override protected def parseRec(state: recursive.ParserState): Result2 = {
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

  final case class OrElseEither[Err, Err2, In, In2, Result, Result2](
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

    override protected def parseRec(state: recursive.ParserState): Either[Result, Result2] = {
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

  final case class OrElse[Err, Err2, In, In2, Result, Result2](
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

    override protected def parseRec(state: recursive.ParserState): Result2 = {
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

    override protected def parseRec(state: recursive.ParserState): Option[Result] = {
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

  final case class Repeat[Err, In, Result](parser: Parser[Err, In, Result], min: Int, max: Option[Int])
      extends Parser[Err, In, Chunk[Result]] {

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

    override protected def parseRec(state: recursive.ParserState): Chunk[Result] = {
      val discard       = state.discard
      val builder       = if (discard) null else ChunkBuilder.make[Result](hint)
      var count         = 0
      var lastItemStart = -1
      val sourceLength  = state.source.length

      while (state.error == null && count < maxCount && lastItemStart < sourceLength) {
        lastItemStart = state.position

        val item = parser.parseRec(state)
        if (state.error == null) {
          count = count + 1
          if (!discard)
            builder += item
        }
      }

      if (count < min) {
        state.error = ParserError.UnexpectedEndOfInput(state.nameStack)
      } else {
        state.error = null
      }

      if (!discard && state.error == null) {
        builder.result()
      } else {
        null.asInstanceOf[Chunk[Result]]
      }
    }

    override protected val needsBacktrack: Boolean = true
  }

  final case class Not[Err, In](parser: Parser[Err, In, Any], failure: Err) extends Parser[Err, In, Unit] {
    override protected def optimizeNode(state: OptimizerState): Parser[Err, In, Unit] =
      Not(parser.runOptimizeNode(state), failure)

    override protected def stripNode(state: OptimizerState): Parser[Err, In, Unit] =
      Not(parser.runStripNode(state), failure)

    override protected def parseRec(state: recursive.ParserState): Unit = {
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

    override protected def parseRec(state: recursive.ParserState): Result = {
      val position = state.position
      val result   = parser.parseRec(state)
      if (state.error != null) {
        state.position = position
      }
      result
    }

    override protected val needsBacktrack: Boolean = false
  }

  final case class SetAutoBacktrack[Err, In, Result](parser: Parser[Err, In, Result], enabled: Boolean)
      extends Parser[Err, In, Result] {

    override protected def optimizeNode(
        state: OptimizerState
    ): Parser[Err, In, Result] =
      parser.runOptimizeNode(state.copy(autoBacktrack = enabled))

    override protected def stripNode(
        state: OptimizerState
    ): Parser[Err, In, Result] = SetAutoBacktrack(parser.runStripNode(state), enabled)

    override protected def parseRec(state: recursive.ParserState): Result =
      ??? // Optimize always removes this node

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

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

    override protected def parseRec(state: recursive.ParserState): Result = {
      val result = parser.parseRec(state)
      if (state.error != null)
        state.error = mapParserErr(state.error.asInstanceOf[ParserError[Err]])
      result
    }

    override protected lazy val needsBacktrack: Boolean = parser.needsBacktrack
  }

  case object Index extends Parser[Nothing, Any, Int] {
    override protected def optimizeNode(state: OptimizerState): Parser[Nothing, Any, Int] = this
    override protected def stripNode(state: OptimizerState): Parser[Nothing, Any, Int]    = this

    override protected def parseRec(state: recursive.ParserState): Int = state.position

    override protected val needsBacktrack: Boolean = false
  }

  case object End extends Parser[Nothing, Any, Unit] {
    override protected def optimizeNode(state: OptimizerState): Parser[Nothing, Any, Unit] = this
    override protected def stripNode(state: OptimizerState): Parser[Nothing, Any, Unit]    = this

    override protected def parseRec(state: recursive.ParserState): Unit =
      if (state.position < state.source.length)
        state.error = ParserError.NotConsumedAll(state.position, None)

    override protected def needsBacktrack: Boolean = false
  }

  /** Parser that does not consume any input and succeeds with 'value' */
  def succeed[Result](value: Result): Parser[Nothing, Any, Result] = Succeed(value)

  /** Parser that always fails with 'failure' */
  def fail[Err](failure: Err): Parser[Err, Any, Nothing] = Fail(failure)

  // Char variants
  /** Parser that consumes the exact character 'value' or fails if it did not match, and results in the unit value. */
  def char(value: Char): Parser[String, Char, Unit] =
    char(value, s"not '$value'")

  /** Parser that consumes the exact character 'value' or fails with 'failure' if it did not match, and results in the
    * unit value
    */
  def char[Err](value: Char, failure: Err): Parser[Err, Char, Unit] =
    regexDiscard(Regex.charIn(value), failure)

  /** Parser that consumes a single character, fails if it is 'value', otherwise results with it */
  def notChar(value: Char): Parser[String, Char, Char] =
    notChar(value, s"cannot be '$value'")

  /** Parser that consumes a single character, fails with 'failure' if it is 'value', otherwise results with it */
  def notChar[Err](value: Char, failure: Err): Parser[Err, Char, Char] =
    regexChar(Regex.charNotIn(value), failure)

  /** Parser that executes a regular expression on the input but discards its result, and fails with 'failure' if the
    * regex fails
    */
  def regexDiscard[Err](regex: Regex, failure: Err): Parser[Err, Char, Unit] =
    Parser.SkipRegex(regex, Some(failure))

  /** Parser that executes a regular expression on the input but discards its result. The regex is supposed to never
    * fail.
    */
  def unsafeRegexDiscard(regex: Regex): Parser[Nothing, Char, Unit] =
    Parser.SkipRegex(regex, None)

  /** Parser that executes a regular expression on the input and results in the last parsed character, or fails with
    * 'failure'. Useful for regexes that are known to parse a single character.
    */
  def regexChar[Err](regex: Regex, failure: Err): Parser.ParseRegexLastChar[Err] =
    Parser.ParseRegexLastChar(regex, Some(failure))

  /** Parser that executes a regular expression on the input and results in the last parsed character. The regex is
    * supposed to never fail. Useful for regexes that are known to parse a single character.
    */
  def unsafeRegexChar[E](regex: Regex): Parser.ParseRegexLastChar[E] =
    Parser.ParseRegexLastChar(regex, None)

  /** Parser that executes a regular expression on the input and results in the chunk of the parsed characters, or fails
    * with 'failure'.
    */
  def regex[Err](regex: Regex, failure: Err): Parser.ParseRegex[Err] =
    Parser.ParseRegex(regex, Some(failure))

  /** Parser that executes a regular expression on the input and results in the chunk of the parsed characters. The
    * regex is supposed to never fail.
    */
  def unsafeRegex(regex: Regex): Parser.ParseRegex[Nothing] =
    Parser.ParseRegex(regex, None)

  /** Parser that consumes a single character and returns it */
  val anyChar: Parser[Nothing, Char, Char] =
    unsafeRegexChar(Regex.anyChar)

  /** Parser that consumes a single character and succeeds with it if it is one of the provided 'chars' */
  def charIn(chars: Char*): Parser[String, Char, Char] =
    regexChar(Regex.charIn(chars: _*), s"Not the expected character (${chars.mkString(", ")})")

  /** Parser that consumes a single character and succeeds with it if it is NOT one of the provided 'chars' */
  def charNotIn(chars: Char*): Parser[String, Char, Char] =
    regexChar(Regex.charNotIn(chars: _*), s"One of the excluded characters (${chars.mkString(", ")})")

  /** Parser that consumes and discards all the remaining input */
  lazy val ignoreRest: Parser[Nothing, Char, Unit] =
    unsafeRegexDiscard(Regex.anyChar.atLeast(0))

  /** Parser that consumes the whole input and captures it as a string */
  val anyString: Parser[Nothing, Char, String] =
    unsafeRegexDiscard(Regex.anyChar.atLeast(0)).string

  /** Parser that requires a given string 'str' and in case it could read it, results in 'value' */
  def string[Result](str: String, value: Result): Parser[String, Char, Result] =
    regexDiscard(Regex.string(str), s"Not '$str'").as(value)

  /** Parser that does not consume the input and results in unit */
  lazy val unit: Parser[Nothing, Any, Unit] = succeed(())

  /** Parser of a single alpha-numeric character */
  lazy val alphaNumeric: Parser[String, Char, Char] = regexChar(Regex.anyAlphaNumeric, "not alphanumeric")

  /** Parser of a single digit */
  lazy val digit: Parser[String, Char, Char] = regexChar(Regex.anyDigit, "not a digit")

  /** Parser of a single letter */
  lazy val letter: Parser[String, Char, Char] = regexChar(Regex.anyLetter, "not a letter")

  /** Parser of a single whitespace character */
  lazy val whitespace: Parser[String, Char, Char] = regexChar(Regex.whitespace, "not a whitespace")

  /** Parser that results in the current input stream position */
  lazy val index: Parser[Nothing, Any, Int] = Parser.Index

  /** Parser that only succeeds if the input stream has been consumed fully.
    *
    * This can be used to require that a parser consumes the full input.
    */
  lazy val end: Parser[Nothing, Any, Unit] = Parser.End

  /** Type representing a parser error */
  sealed trait ParserError[+Err] { self =>
    def addFailedBranch[Err2 >: Err](error: ParserError[Err2]): ParserError[Err2] =
      ParserError.AllBranchesFailed(self, error)

    def map[Err2](f: Err => Err2): ParserError[Err2] = self match {
      case ParserError.Failure(nameStack, position, failure) => ParserError.Failure(nameStack, position, f(failure))
      case ParserError.UnknownFailure(nameStack, position)   => ParserError.UnknownFailure(nameStack, position)
      case ParserError.UnexpectedEndOfInput(nameStack)       => ParserError.UnexpectedEndOfInput(nameStack)
      case ParserError.NotConsumedAll(position, lastFailure) =>
        ParserError.NotConsumedAll(position, lastFailure.map(_.map(f)))
      case ParserError.AllBranchesFailed(left, right)        => ParserError.AllBranchesFailed(left.map(f), right.map(f))
    }
  }

  object ParserError {

    /** User-defined parser error of type 'Err'
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

    /** Unknown parser error. This is only produced in exceptional cases that should not happen, for example if the
      * unsafe regex variants encounter an error.
      *
      * @param nameStack
      *   Stack of named parsers until reaching the failure
      * @param position
      *   Input stream position
      */
    final case class UnknownFailure(nameStack: List[String], position: Int) extends ParserError[Nothing]

    /** The input stream ended before the parser finished */
    final case class UnexpectedEndOfInput(nameStack: List[String]) extends ParserError[Nothing]

    /** The parser was supposed to consume the full input but it did not.
      *
      * @param lastFailure
      *   the last encountered failure, if any
      */
    final case class NotConsumedAll[Err](position: Int, lastFailure: Option[ParserError[Err]]) extends ParserError[Err]

    /** All branches failed in a sequence of orElse or orElseEither parsers.
      *
      * Every failed branch's failure is preserved.
      */
    final case class AllBranchesFailed[Err](left: ParserError[Err], right: ParserError[Err]) extends ParserError[Err]
  }
}
