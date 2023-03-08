package zio.parser

import zio.parser.Parser.ParserError
import zio.parser.internal.PrinterImpl
import zio.parser.target.{ChunkTarget, Target}
import zio.{=!=, Chunk}

/** A Printer takes a value of type 'Value' and either produces a stream of 'Out' elements and a result value of type
  * 'Result', or fails with a custom error of type 'Err'.
  *
  * Parsers can be combined with Printers to get Syntax, or a Parser and a Printer can be built simultaneously by using
  * the combinators of Syntax.
  *
  * @tparam Err
  *   Custom error type
  * @tparam Out
  *   Element type of the output stream of printing
  * @tparam Value
  *   The type of the value to be printed
  */
sealed trait Printer[+Err, +Out, -Value] extends VersionSpecificPrinter[Err, Out, Value] { self =>

  /** Maps the printer's result with function 'to' and its input value with 'from'. Both functions can fail the printer.
    */
  final def contramapEither[Err2, Value2](
      from: Value2 => Either[Err2, Value]
  ): Printer[Err2, Out, Value2] =
    Printer.ContramapEither(self, from)

  /** Maps the printer's result with function 'to' and its input value with 'from'. Both functions can fail the printer.
    * Failure is indicated by None on the error channel.
    */
  final def transformOption[Value2](
      from: Value2 => Option[Value]
  ): Printer[Option[Err], Out, Value2] =
    contramapEither[Option[Err], Value2](value => from(value).toRight(None))

  /** Maps the result of the printer with the function 'to', and the value to be printed with the partial function
    * 'from'. It the partial function is not defined on the value, the printer fails with 'failure'.
    *
    * This can be used to define separate syntaxes for subtypes, that can be later combined.
    */
  final def contramapTo[Err2 >: Err, Value2](
      from: PartialFunction[Value2, Value],
      failure: Err2
  ): Printer[Err2, Out, Value2] =
    contramapEither((d0: Value2) => from.lift(d0).fold[Either[Err2, Value]](Left(failure))(Right.apply))

  /** Symbolic alias for zipLeft */
  final def <~[Err2 >: Err, Out2 >: Out](
      that: => Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Value] =
    zipLeft(that)

  /** Print 'that' by providing the unit value to it after printing this. The result is this printer's result. */
  final def zipLeft[Err2 >: Err, Out2 >: Out](
      that: => Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Value] =
    Printer.ZipLeft(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Specifies a filter condition 'condition' that gets checked on both the printed and the result value and in case it
    * evaluates to false, fails with 'failure'.
    */
  final def filter[E2 >: Err, Value2 <: Value](condition: Value2 => Boolean, failure: E2): Printer[E2, Out, Value2] =
    contramapEither((d1: Value2) => if (condition(d1)) Right(d1) else Left(failure))

  /** Symbolic alias for orElse */
  final def |[Err2 >: Err, Out2 >: Out, Value2 <: Value](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Value2] = orElse(that)

  /** Symbolic alias for orElse */
  final def <>[Err2 >: Err, Out2 >: Out, Value2 <: Value](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Value2] = orElse(that)

  /** Prints this and if it fails, ignore the printed output and print 'that' instead. */
  final def orElse[Err2 >: Err, Out2 >: Out, Value2 <: Value](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Value2] =
    Printer.OrElse(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Symbolic alias for orElseEither */
  final def <+>[Err2 >: Err, Out2 >: Out, Value2](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Either[Value, Value2]] =
    orElseEither(that)

  /** Prints this if the input is 'Left', or print 'that' if the input is 'Right' */
  final def orElseEither[Err2 >: Err, Out2 >: Out, Value2](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Either[Value, Value2]] =
    Printer.OrElseEither(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Repeats this printer for each element of the input chunk, assuming it has at least one element */
  final def repeat: Printer[Err, Out, Chunk[Value]] =
    Printer.Repeat(self, 1, None)

  /** Symbolic alias for repeat */
  final def + : Printer[Err, Out, Chunk[Value]] = repeat

  /** Repeats this printer for each element of the input chunk, zero or more times */
  final def repeat0: Printer[Err, Out, Chunk[Value]] =
    Printer.Repeat(self, 0, None)

  /** Symbolic alias for repeat0 */
  final def * : Printer[Err, Out, Chunk[Value]] = repeat0

  /** Repeats this printer for each element of the input chunk, separated by the 'sep' printer (which gets Unit to be
    * printed)
    */
  final def repeatWithSep[Err2 >: Err, Out2 >: Out](
      sep: Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Chunk[Value]] =
    (self ~ (sep ~> self).repeat0).contramap(chunk => (chunk.head, chunk.tail))

  /** Repeats this printer for each element of the input chunk that can be empty, separated by the 'sep' printer (which
    * gets Unit to be printed)
    */
  final def repeatWithSep0[Err2 >: Err, Out2 >: Out](
      sep: Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Chunk[Value]] =
    (self ~ (sep ~> self).repeat0).optional
      .contramap(chunk => chunk.headOption.map(head => (head, chunk.tail)))

  /** Repeat this printer for each element of the input chunk, verifying the 'stopConfition' after each. */
  final def repeatUntil[Err2 >: Err, Out2 >: Out](
      stopCondition: Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Chunk[Value]] =
    self.repeat0 <~ stopCondition

  /** Print option values */
  final def optional: Printer[Err, Out, Option[Value]] =
    Printer.Optional(self)

  /** Symbolic alias for optional */
  final def ? : Printer[Err, Out, Option[Value]] = optional

  /** Surround this printer with 'left' and 'right', each getting Unit as value to be printed. */
  final def between[Err2 >: Err, Out2 >: Out](
      left: Printer[Err2, Out2, Unit],
      right: Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Value] =
    (left ~ self ~ right).contramap((value: Value) => value)

  /** Surround this printer with 'other', which will get Unit as value to be printed. */
  final def surroundedBy[Err2 >: Err, Out2 >: Out](
      other: Printer[Err2, Out2, Unit]
  ): Printer[Err2, Out2, Value] =
    (other ~ self ~ other).contramap((value: Value) => value)

  /** Maps the error with function 'f' */
  final def mapError[Err2](f: Err => Err2): Printer[Err2, Out, Value] =
    Printer.MapError[Err, Err2, Out, Value](self, f)

  /** Concatenates a chunk of string to be printed */
  final def flatten(implicit
      ev1: Chunk[String] <:< Value
  ): Printer[Err, Out, String] =
    contramap((s: String) => ev1(Chunk(s)))

  /** Widen this printer's printed value type by specifying a partial function to narrow it back to the set of supported
    * subtypes.
    */
  final def widenWith[Err2 >: Err, Value2](
      narrow: PartialFunction[Value2, Value],
      failure: Err2
  ): Printer[Err2, Out, Value2] =
    contramapTo(narrow, failure)

  /** Ignores the printer's result and input and use 'result' and 'value' instead */
  final def asPrinted[Value2](matches: Value2, value: Value): Printer[Err, Out, Value2] =
    Printer.Ignore[Err, Out, Value, Value2](self, matches, value)

  /** Maps the printer's input value with function 'f' */
  final def contramap[Value2](f: Value2 => Value): Printer[Err, Out, Value2] =
    contramapEither(f.andThen(Right.apply))

  /** Provide this printer it's input value */
  final def apply(value: Value): Printer[Err, Out, Any] =
    Printer.ProvideValue(self, value)

  // Execution
  /** Print the given 'value' to the given 'target' implementation */
  final def print[Out2 >: Out](value: Value, target: Target[Out2]): Either[Err, Unit] = {
    val printer = new PrinterImpl[Err, Out2, Value](self)
    printer.run(value, target)
  }

  /** Print the given 'value' to a chunk of output elements */
  final def print(value: Value): Either[Err, Chunk[Out]] = {
    val target = new ChunkTarget[Out]
    self.print(value, target).map(_ => target.result)
  }

  /** Print the given 'value' to a string */
  final def printString(value: Value)(implicit ev: Out <:< Char): Either[Err, String] =
    self.print(value).map(_.mkString)
}

object Printer {
  val unit: Printer[Nothing, Nothing, Any] = Printer.Succeed(())

  final case class Lazy[Err, Out, Value](inner: () => Printer[Err, Out, Value]) extends Printer[Err, Out, Value] {
    lazy val memoized: Printer[Err, Out, Value] = inner()
  }

  final case class Succeed(value: Any) extends Printer[Nothing, Nothing, Any]

  final case class Fail[+Err](failure: Err) extends Printer[Err, Nothing, Any]

  final case class Failed[+Err](failure: ParserError[Err]) extends Printer[Err, Nothing, Any]

  final case class ProvideValue[Err, Out, Value](printer: Printer[Err, Out, Value], value: Value)
      extends Printer[Err, Out, Any]

  final case class Passthrough[-Value, +Result]() extends Printer[Nothing, Result, Value]

  final case class SkipRegex(regex: Regex, printAs: Chunk[Char]) extends Printer[Nothing, Char, Unit] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class ParseRegex[Err](regex: Regex, onFailure: Option[Err]) extends Printer[Err, Char, Chunk[Char]] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class ParseRegexLastChar[Err](regex: Regex, onFailure: Option[Err]) extends Printer[Err, Char, Char] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class ContramapEither[Err, Err2, Out, Value2, Value](
      printer: Printer[Err, Out, Value],
      from: Value2 => Either[Err2, Value]
  ) extends Printer[Err2, Out, Value2]

  final case class Contramap[Err, Err2, Out, Value2, Value](
      printer: Printer[Err, Out, Value],
      from: Value2 => Value
  ) extends Printer[Err2, Out, Value2]

  final case class Ignore[Err, Out, Value, Value2](
      printer: Printer[Err, Out, Value],
      matches: Value2,
      from: Value
  ) extends Printer[Err, Out, Value2]

  final case class Zip[Err, Err2, Out, Out2, Value2, Value, ZippedValue](
      left: Printer[Err, Out, Value],
      right: Printer[Err2, Out2, Value2],
      unzipValue: ZippedValue => (Value, Value2)
  ) extends Printer[Err2, Out2, ZippedValue]

  final case class ZipLeft[Err, Err2, Out, Out2, Value2, Value](
      left: Printer[Err, Out, Value],
      right: Printer[Err2, Out2, Value2]
  ) extends Printer[Err2, Out2, Value]

  final case class ZipRight[Err, Err2, Out, Out2, Value2, Value](
      left: Printer[Err, Out, Value],
      right: Printer[Err2, Out2, Value2]
  ) extends Printer[Err2, Out2, Value2]

  final case class FlatMapValue[Err, Out, Value](f: Value => Printer[Err, Out, Nothing])
      extends Printer[Err, Out, Value]

  final case class OrElseEither[Err, Err2, Out, Out2, Value2, Value](
      left: Printer[Err, Out, Value],
      right: Printer[Err2, Out2, Value2]
  ) extends Printer[Err2, Out2, Either[Value, Value2]]

  final case class OrElse[Err, Err2, Out, Out2, Value2, Value](
      left: Printer[Err, Out, Value],
      right: Printer[Err2, Out2, Value2]
  ) extends Printer[Err2, Out2, Value2]

  final case class Optional[Err, Out, Value](printer: Printer[Err, Out, Value]) extends Printer[Err, Out, Option[Value]]

  final case class Repeat[Err, Out, Value](
      printer: Printer[Err, Out, Value],
      min: Int,
      max: Option[Int]
  ) extends Printer[Err, Out, Chunk[Value]]

  final case class MapError[Err, Err2, Out, Value](
      printer: Printer[Err, Out, Value],
      mapPrinterErr: Err => Err2
  ) extends Printer[Err2, Out, Value]

  /** Printer that does not print anything and fails with 'failure' */
  def fail[Err](failure: Err): Printer[Err, Nothing, Any] = Fail(failure)

  /** Printer that just emits its input value */
  def any[T]: Printer[Nothing, T, T] = Passthrough[T, T]()

  /** Printer determined by a function on the input value */
  def byValue[Err, Out, Value](f: Value => Printer[Err, Out, Nothing]): Printer[Err, Out, Value] =
    Printer.FlatMapValue[Err, Out, Value](f)

  /** Printer emitting a specific value */
  def print[Out](value: Out): Printer[Nothing, Out, Any] =
    Printer.ProvideValue(Printer.Passthrough(), value)

  /** Printer emitting a specific string to a char output */
  def printString(value: String): Printer[Nothing, Char, Any] =
    Printer.anyString(value)

  // Char variants
  /** Printer that prints a given character and results in unit */
  def char(value: Char): Printer[String, Char, Unit] =
    regexDiscard(Regex.charIn(value), Chunk(value))

  /** Printer that prints the input character if it does not equal to 'value', otherwise fails */
  def notChar(value: Char): Printer[String, Char, Char] =
    notChar(value, s"cannot be '$value'")

  /** Printer that prints the input character if it does not equal to 'value', otherwise fails with 'failure' */
  def notChar[Err](value: Char, failure: Err): Printer[Err, Char, Char] =
    regexChar(Regex.charNotIn(value), failure)

  // Generic variants
  /** Printer that emits the given value */
  def apply[Out](value: => Out)(implicit ev: Out =!= Char): Printer[String, Out, Unit] =
    exactly(value).apply(value)

  /** Printer that emits the input if it is equals to 'value'. Otherwise fails */
  def exactly[Out](value: Out)(implicit ev: Out =!= Char): Printer[String, Out, Out] =
    exactly(value, s"not '$value'")

  /** Printer that emits the input if it is equals to 'value'. Otherwise fails with 'failure' */
  def exactly[Err, Out](value: Out, failure: Err)(implicit ev: Out =!= Char): Printer[Err, Out, Out] =
    any.filter(_ == value, failure)

  /** Printer that emits the input except if it is equals to 'value', in which case it fails */
  def except[Out](value: Out)(implicit ev: Out =!= Char): Printer[String, Out, Out] =
    except(value, s"cannot be '$value'")

  /** Printer that emits the input except if it is equals to 'value', in which case it fails with 'failure' */
  def except[Err, Out](value: Out, failure: Err)(implicit ev: Out =!= Char): Printer[Err, Out, Out] =
    any.filter(_ != value, failure)

  /** Printer that prints the given chunk of characters in 'value' */
  def regexDiscard(regex: Regex, value: Chunk[Char]): Printer[Nothing, Char, Unit] =
    Printer.SkipRegex(regex, value)

  /** Printer that prints a single character if matches the given 'regex', otherwise fails with 'failure' */
  def regexChar[Err](regex: Regex, failure: Err): Printer[Err, Char, Char] =
    Printer.ParseRegexLastChar(regex, Some(failure))

  /** Printer that prints a single character if matches the given 'regex'. The regex should never fail. */
  def unsafeRegexChar(regex: Regex): Printer[Nothing, Char, Char] =
    Printer.ParseRegexLastChar(regex, None)

  /** Printer that prints a series of characters provided as input, if it matches the given regex. Otherwise fails with
    * 'failure'
    */
  def regex[Err](regex: Regex, failure: Err): Printer[Err, Char, Chunk[Char]] =
    Printer.ParseRegex(regex, Some(failure))

  /** Printer that prints a series of characters provided as input, if it matches the given regex. The regex should
    * never fail.
    */
  def unsafeRegex[Err](regex: Regex): Printer[Err, Char, Chunk[Char]] =
    Printer.ParseRegex(regex, None)

  /** Printer that prints a single character provided as input. */
  val anyChar: Printer[Nothing, Char, Char] =
    unsafeRegexChar(Regex.anyChar)

  /** Printer that prints a single character if it matches any of the given 'chars' */
  def charIn(chars: Char*): Printer[String, Char, Char] =
    regexChar(Regex.charIn(chars: _*), s"Not the expected character (${chars.mkString(", ")})")

  /** Printer that prints a single character except if it matches any of the given 'chars' */
  def charNotIn(chars: Char*): Printer[String, Char, Char] =
    regexChar(Regex.charNotIn(chars: _*), s"One of the excluded characters (${chars.mkString(", ")})")

  /** Printer that just prints the input string */
  val anyString: Printer[Nothing, Char, String] =
    unsafeRegex(Regex.anyChar.atLeast(0))
      .contramap((s: String) => Chunk.fromArray(s.toCharArray))

  /** Prints a specific string 'str' and results in 'value' */
  def string[Result](str: String, value: Result): Printer[Nothing, Char, Result] =
    regexDiscard(Regex.string(str), Chunk.fromArray(str.toCharArray)).asPrinted(value, ())

  /** Prints a single alpha-numeric character */
  val alphaNumeric: Printer[String, Char, Char] = regexChar(Regex.anyAlphaNumeric, "not alphanumeric")

  /** Prints a single digit */
  val digit: Printer[String, Char, Char] = regexChar(Regex.anyDigit, "not a digit")

  /** Prints a single letter */
  val letter: Printer[String, Char, Char] = regexChar(Regex.anyLetter, "not a letter")

  /** Prints a single whitespace character */
  val whitespace: Printer[String, Char, Char] = regexChar(Regex.whitespace, "not a whitespace")
}
