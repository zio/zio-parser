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
  * @tparam Result
  *   The type of the parsed result value
  */
sealed trait Printer[+Err, +Out, -Value, +Result] { self =>

  def named(name: String): Printer[Err, Out, Value, Result] =
    Printer.Named(name, self)

  /** Maps the printer's result with function 'to' and its input value with 'from' */
  final def transform[Value2, Result2](
      to: Result => Result2,
      from: Value2 => Value
  ): Printer[Err, Out, Value2, Result2] =
    Printer.Transform(self, to, from)

  /** Maps the printer's result with function 'to' and its input value with 'from'. Both functions can fail the printer.
    */
  final def transformEither[Err2, Value2, Result2](
      to: Result => Either[Err2, Result2],
      from: Value2 => Either[Err2, Value]
  ): Printer[Err2, Out, Value2, Result2] =
    Printer.TransformEither(self, to, from)

  /** Maps the printer's result with function 'to' and its input value with 'from'. Both functions can fail the printer.
    * Failure is indicated by None on the error channel.
    */
  final def transformOption[Value2, Result2](
      to: Result => Option[Result2],
      from: Value2 => Option[Value]
  ): Printer[Option[Err], Out, Value2, Result2] =
    transformEither[Option[Err], Value2, Result2](
      value => to(value).toRight(None),
      value => from(value).toRight(None)
    )

  /** Maps the result of the printer with the function 'to', and the value to be printed with the partial function
    * 'from'. It the partial function is not defined on the value, the printer fails with 'failure'.
    *
    * This can be used to define separate syntaxes for subtypes, that can be later combined.
    */
  final def transformTo[Err2 >: Err, Value2, Result2](
      to: Result => Result2,
      from: PartialFunction[Value2, Value],
      failure: Err2
  ): Printer[Err2, Out, Value2, Result2] =
    transformEither(
      to.andThen(Right.apply),
      (d0: Value2) => from.lift(d0).fold[Either[Err2, Value]](Left(failure))(Right.apply)
    )

  /** Symbolic alias for zipLeft */
  final def <~[Err2 >: Err, Out2 >: Out](
      that: => Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Value, Result] =
    zipLeft(that)

  /** Print 'that' by providing the unit value to it after printing this. The result is this printer's result. */
  final def zipLeft[Err2 >: Err, Out2 >: Out](
      that: => Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Value, Result] =
    Printer.ZipLeft(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Use this printer's result value to determine the next printer */
  final def flatMap[E2 >: Err, O2 >: Out, D0 <: Value, D3](
      that: Result => Printer[E2, O2, D0, D3]
  ): Printer[E2, O2, D0, D3] =
    Printer.FlatMapResult(self, that)

  /** Specifies a filter condition 'condition' that gets checked on both the printed and the result value and in case it
    * evaluates to false, fails with 'failure'.
    */
  final def filter[E2 >: Err, Result2 <: Value](condition: Result2 => Boolean, failure: E2)(implicit
      ev: Result <:< Result2
  ): Printer[E2, Out, Result2, Result2] =
    transformEither(
      (d2: Result) => if (condition(ev(d2))) Right(d2) else Left(failure),
      (d1: Result2) => if (condition(d1)) Right(d1) else Left(failure)
    )

  /** Symbolic alias for orElse */
  final def |[Err2 >: Err, Out2 >: Out, Value2 <: Value, Result2 >: Result](
      that: => Printer[Err2, Out2, Value2, Result2]
  ): Printer[Err2, Out2, Value2, Result2] = orElse(that)

  /** Symbolic alias for orElse */
  final def <>[Err2 >: Err, Out2 >: Out, Value2 <: Value, Result2 >: Result](
      that: => Printer[Err2, Out2, Value2, Result2]
  ): Printer[Err2, Out2, Value2, Result2] = orElse(that)

  /** Prints this and if it fails, ignore the printed output and print 'that' instead. */
  final def orElse[Err2 >: Err, Out2 >: Out, Value2 <: Value, Result2 >: Result](
      that: => Printer[Err2, Out2, Value2, Result2]
  ): Printer[Err2, Out2, Value2, Result2] =
    Printer.OrElse(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Symbolic alias for orElseEither */
  final def <+>[Err2 >: Err, Out2 >: Out, Value2, Result2](
      that: => Printer[Err2, Out2, Value2, Result2]
  ): Printer[Err2, Out2, Either[Value, Value2], Either[Result, Result2]] =
    orElseEither(that)

  /** Prints this if the input is 'Left', or print 'that' if the input is 'Right' */
  final def orElseEither[Err2 >: Err, Out2 >: Out, Value2, Result2](
      that: => Printer[Err2, Out2, Value2, Result2]
  ): Printer[Err2, Out2, Either[Value, Value2], Either[Result, Result2]] =
    Printer.OrElseEither(Printer.Lazy(() => self), Printer.Lazy(() => that))

  /** Repeats this printer for each element of the input chunk, assuming it has at least one element */
  final def repeat: Printer[Err, Out, Chunk[Value], Chunk[Result]] =
    Printer.Repeat(self, 1, None)

  /** Symbolic alias for repeat */
  final def + : Printer[Err, Out, Chunk[Value], Chunk[Result]] = repeat

  /** Repeats this printer for each element of the input chunk, zero or more times */
  final def repeat0: Printer[Err, Out, Chunk[Value], Chunk[Result]] =
    Printer.Repeat(self, 0, None)

  /** Symbolic alias for repeat0 */
  final def * : Printer[Err, Out, Chunk[Value], Chunk[Result]] = repeat0

  /** Repeats this printer for each element of the input chunk, separated by the 'sep' printer (which gets Unit to be
    * printed)
    */
  final def repeatWithSep[Err2 >: Err, Out2 >: Out](
      sep: Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Chunk[Value], Chunk[Result]] =
    (self ~ (sep ~> self).repeat0).transform(
      { case (head, tail) => head +: tail },
      chunk => (chunk.head, chunk.tail)
    )

  /** Repeats this printer for each element of the input chunk that can be empty, separated by the 'sep' printer (which
    * gets Unit to be printed)
    */
  final def repeatWithSep0[Err2 >: Err, Out2 >: Out](
      sep: Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Chunk[Value], Chunk[Result]] =
    (self ~ (sep ~> self).repeat0).optional
      .transform(
        {
          case Some((head, tail)) => head +: tail
          case None               => Chunk.empty
        },
        chunk => chunk.headOption.map(head => (head, chunk.tail))
      )

  /** Repeat this printer for each element of the input chunk, verifying the 'stopConfition' after each. */
  final def repeatUntil[Err2 >: Err, Out2 >: Out](
      stopCondition: Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Chunk[Value], Chunk[Result]] =
    self.repeat0 <~ stopCondition

  /** Print option values */
  final def optional: Printer[Err, Out, Option[Value], Option[Result]] =
    Printer.Optional(self)

  /** Symbolic alias for optional */
  final def ? : Printer[Err, Out, Option[Value], Option[Result]] = optional

  /** Surround this printer with 'left' and 'right', each getting Unit as value to be printed. */
  final def between[Err2 >: Err, Out2 >: Out](
      left: Printer[Err2, Out2, Unit, Any],
      right: Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Value, Result] =
    (left ~ self ~ right).transform(
      { case (_, value, _) => value },
      (value: Value) => value
    )

  /** Surround this printer with 'other', which will get Unit as value to be printed. */
  final def surroundedBy[Err2 >: Err, Out2 >: Out](
      other: Printer[Err2, Out2, Unit, Any]
  ): Printer[Err2, Out2, Value, Result] =
    (other ~ self ~ other).transform(
      { case (_, value, _) => value },
      (value: Value) => value
    )

  /** Maps the error with function 'f' */
  final def mapError[Err2](f: Err => Err2): Printer[Err2, Out, Value, Result] =
    Printer.MapError[Err, Err2, Out, Value, Result](
      self,
      f
    )

  /** Concatenates a chunk of string to be printed */
  final def flatten(implicit
      ev1: Chunk[String] <:< Value,
      ev2: Result <:< Chunk[String]
  ): Printer[Err, Out, String, String] =
    transform(s => s.mkString, (s: String) => ev1(Chunk(s)))

  /** Widen this printer's printed value type by specifying a partial function to narrow it back to the set of supported
    * subtypes.
    */
  final def widenWith[Err2 >: Err, Result2](narrow: PartialFunction[Result2, Value], failure: Err2)(implicit
      ev: Result <:< Result2
  ): Printer[Err2, Out, Result2, Result2] =
    transformTo(
      (value: Result) => ev(value),
      narrow,
      failure
    )

  /** Maps the result of this printer by 'f' */
  final def map[Result2](f: Result => Result2): Printer[Err, Out, Value, Result2] =
    self.transformEither(f.andThen(Right.apply), (value: Value) => Right(value))

  /** Ignores the result of this printer and use 'result' instead */
  final def as[Result2](result: => Result2): Printer[Err, Out, Value, Result2] =
    self.map(_ => result)

  /** Ignores the printer's result and input and use 'result' and 'value' instead */
  final def asPrinted[Result2](result: Result2, value: Value): Printer[Err, Out, Result2, Result2] =
    Printer.Ignore(self, result, value)

  /** Ignores the result of this printer and return unit instead */
  final def unit: Printer[Err, Out, Value, Unit] = as(())

  /** Maps the printer's input value with function 'f' */
  final def contramap[Value2](f: Value2 => Value): Printer[Err, Out, Value2, Result] =
    transformEither(
      (value: Result) => Right(value),
      f.andThen(Right.apply)
    )

  /** Provide this printer it's input value */
  final def apply(value: Value): Printer[Err, Out, Any, Result] =
    Printer.ProvideValue(self, value)

  // Execution
  /** Print the given 'value' to the given 'target' implementation */
  final def print[Out2 >: Out](value: Value, target: Target[Out2], colorMap: Map[String, String]): Either[Err, Unit] = {
    val printer = new PrinterImpl[Err, Out2, Value, Result](self, colorMap)
    printer.run(value, target)
  }

  /** Print the given 'value' to a chunk of output elements */
  final def print(value: Value, colorMap: Map[String, String]): Either[Err, Chunk[Out]] = {
    val target = new ChunkTarget[Out]
    self.print(value, target, colorMap).map(_ => target.result)
  }

  /** Print the given 'value' to a string */
  final def printString(value: Value, colorMap: Map[String, String] = Map.empty)(implicit
      ev: Out <:< Char
  ): Either[Err, String] =
    self.print(value, colorMap).map(_.mkString)
}

object Printer {
  final case class Lazy[Err, Out, Value, Result](inner: () => Printer[Err, Out, Value, Result])
      extends Printer[Err, Out, Value, Result] {

    lazy val memoized: Printer[Err, Out, Value, Result] = inner()
  }

  final case class Named[Err, Out, Value, Result](name: String, inner: Printer[Err, Out, Value, Result])
      extends Printer[Err, Out, Value, Result]

  final case class Succeed[+Result](value: Result) extends Printer[Nothing, Nothing, Any, Result]

  final case class Fail[+Err](failure: Err) extends Printer[Err, Nothing, Any, Nothing]

  final case class Failed[+Err](failure: ParserError[Err]) extends Printer[Err, Nothing, Any, Nothing]

  final case class ProvideValue[Err, Out, Value, Result](printer: Printer[Err, Out, Value, Result], value: Value)
      extends Printer[Err, Out, Any, Result]

  final case class Passthrough[-Value, +Result]() extends Printer[Nothing, Result, Value, Result]

  final case class SkipRegex(regex: Regex, printAs: Chunk[Char]) extends Printer[Nothing, Char, Unit, Unit] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class ParseRegex[Err](regex: Regex, onFailure: Option[Err])
      extends Printer[Err, Char, Chunk[Char], Chunk[Char]] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class ParseRegexLastChar[Err](regex: Regex, onFailure: Option[Err])
      extends Printer[Err, Char, Char, Char] {
    lazy val compiledRegex: Regex.Compiled = regex.compile
  }

  final case class TransformEither[Err, Err2, Out, Value2, Value, Result, Result2](
      printer: Printer[Err, Out, Value, Result],
      to: Result => Either[Err2, Result2],
      from: Value2 => Either[Err2, Value]
  ) extends Printer[Err2, Out, Value2, Result2]

  final case class Transform[Err, Err2, Out, Value2, Value, Result, Result2](
      printer: Printer[Err, Out, Value, Result],
      to: Result => Result2,
      from: Value2 => Value
  ) extends Printer[Err2, Out, Value2, Result2]

  final case class Ignore[Err, Err2, Out, Value2, Value, Result, Result2](
      printer: Printer[Err, Out, Value, Result],
      to: Result2,
      from: Value
  ) extends Printer[Err2, Out, Value2, Result2]

  final case class Zip[Err, Err2, Out, Out2, Value2, Value, Result, Result2, ZippedValue, ZippedResult](
      left: Printer[Err, Out, Value, Result],
      right: Printer[Err2, Out2, Value2, Result2],
      unzipValue: ZippedValue => (Value, Value2),
      zipResult: (Result, Result2) => ZippedResult
  ) extends Printer[Err2, Out2, ZippedValue, ZippedResult]

  final case class ZipLeft[Err, Err2, Out, Out2, Value2, Value, Result, Result2](
      left: Printer[Err, Out, Value, Result],
      right: Printer[Err2, Out2, Value2, Result2]
  ) extends Printer[Err2, Out2, Value, Result]

  final case class ZipRight[Err, Err2, Out, Out2, Value2, Value, Result, Result2](
      left: Printer[Err, Out, Value, Result],
      right: Printer[Err2, Out2, Value2, Result2]
  ) extends Printer[Err2, Out2, Value2, Result2]

  final case class FlatMapValue[Err, Out, Value, Result](f: Value => Printer[Err, Out, Any, Result])
      extends Printer[Err, Out, Value, Result]

  final case class FlatMapResult[Err, Err2, Out, Out2, Value2, Value, Result, Result2](
      printer: Printer[Err, Out, Value, Result],
      f: Result => Printer[Err2, Out2, Value2, Result2]
  ) extends Printer[Err2, Out2, Value2, Result2]

  final case class OrElseEither[Err, Err2, Out, Out2, Value2, Value, Result, Result2](
      left: Printer[Err, Out, Value, Result],
      right: Printer[Err2, Out2, Value2, Result2]
  ) extends Printer[Err2, Out2, Either[Value, Value2], Either[Result, Result2]]

  final case class OrElse[Err, Err2, Out, Out2, Value2, Value, Result, Result2](
      left: Printer[Err, Out, Value, Result],
      right: Printer[Err2, Out2, Value2, Result2]
  ) extends Printer[Err2, Out2, Value2, Result2]

  final case class Optional[Err, Out, Value, Result](printer: Printer[Err, Out, Value, Result])
      extends Printer[Err, Out, Option[Value], Option[Result]]

  final case class Repeat[Err, Out, Value, Result](
      printer: Printer[Err, Out, Value, Result],
      min: Int,
      max: Option[Int]
  ) extends Printer[Err, Out, Chunk[Value], Chunk[Result]]

  final case class MapError[Err, Err2, Out, Value, Result](
      printer: Printer[Err, Out, Value, Result],
      mapPrinterErr: Err => Err2
  ) extends Printer[Err2, Out, Value, Result]

  /** Printer that does not print anything but results in 'value' */
  def succeed[Result](value: Result): Printer[Nothing, Nothing, Any, Result] = Succeed(value)

  /** Printer that does not print anything and fails with 'failure' */
  def fail[Err](failure: Err): Printer[Err, Nothing, Any, Nothing] = Fail(failure)

  /** Printer that just emits its input value */
  def any[Result]: Printer[Nothing, Result, Result, Result] = Passthrough()

  /** Printer determined by a function on the input value */
  def byValue[Err, Out, Value, Result](f: Value => Printer[Err, Out, Any, Result]): Printer[Err, Out, Value, Result] =
    Printer.FlatMapValue[Err, Out, Value, Result](f)

  /** Printer emitting a specific value */
  def print[Out](value: Out): Printer[Nothing, Out, Any, Unit] =
    Printer.ProvideValue(Printer.Passthrough(), value)

  /** Printer emitting a specific string to a char output */
  def printString(value: String): Printer[Nothing, Char, Any, Unit] =
    Printer.anyString(value).unit

  // Char variants
  /** Printer that prints a given character and results in unit */
  def char(value: Char): Printer[String, Char, Unit, Unit] =
    regexDiscard(Regex.charIn(value), Chunk(value))

  /** Printer that prints the input character if it does not equal to 'value', otherwise fails */
  def notChar(value: Char): Printer[String, Char, Char, Char] =
    notChar(value, s"cannot be '$value'")

  /** Printer that prints the input character if it does not equal to 'value', otherwise fails with 'failure' */
  def notChar[Err](value: Char, failure: Err): Printer[Err, Char, Char, Char] =
    regexChar(Regex.charNotIn(value), failure)

  // Generic variants
  /** Printer that emits the given value */
  def apply[Out](value: => Out)(implicit ev: Out =!= Char): Printer[String, Out, Unit, Unit] =
    exactly(value).asPrinted((), value)

  /** Printer that emits the input if it is equals to 'value'. Otherwise fails */
  def exactly[Out](value: Out)(implicit ev: Out =!= Char): Printer[String, Out, Out, Out] =
    exactly(value, s"not '$value'")

  /** Printer that emits the input if it is equals to 'value'. Otherwise fails with 'failure' */
  def exactly[Err, Out](value: Out, failure: Err)(implicit ev: Out =!= Char): Printer[Err, Out, Out, Out] =
    any[Out].filter(_ == value, failure)

  /** Printer that emits the input except if it is equals to 'value', in which case it fails */
  def except[Out](value: Out)(implicit ev: Out =!= Char): Printer[String, Out, Out, Out] =
    except(value, s"cannot be '$value'")

  /** Printer that emits the input except if it is equals to 'value', in which case it fails with 'failure' */
  def except[Err, Out](value: Out, failure: Err)(implicit ev: Out =!= Char): Printer[Err, Out, Out, Out] =
    any[Out].filter(_ != value, failure)

  /** Printer that prints the given chunk of characters in 'value' */
  def regexDiscard(regex: Regex, value: Chunk[Char]): Printer[Nothing, Char, Unit, Unit] =
    Printer.SkipRegex(regex, value)

  /** Printer that prints a single character if matches the given 'regex', otherwise fails with 'failure' */
  def regexChar[Err](regex: Regex, failure: Err): Printer[Err, Char, Char, Char] =
    Printer.ParseRegexLastChar(regex, Some(failure))

  /** Printer that prints a single character if matches the given 'regex'. The regex should never fail. */
  def unsafeRegexChar(regex: Regex): Printer[Nothing, Char, Char, Char] =
    Printer.ParseRegexLastChar(regex, None)

  /** Printer that prints a series of characters provided as input, if it matches the given regex. Otherwise fails with
    * 'failure'
    */
  def regex[Err](regex: Regex, failure: Err): Printer[Err, Char, Chunk[Char], Chunk[Char]] =
    Printer.ParseRegex(regex, Some(failure))

  /** Printer that prints a series of characters provided as input, if it matches the given regex. The regex should
    * never fail.
    */
  def unsafeRegex[Err](regex: Regex): Printer[Err, Char, Chunk[Char], Chunk[Char]] =
    Printer.ParseRegex(regex, None)

  /** Printer that prints a single character provided as input. */
  val anyChar: Printer[Nothing, Char, Char, Char] =
    unsafeRegexChar(Regex.anyChar)

  /** Printer that prints a single character if it matches any of the given 'chars' */
  def charIn(chars: Char*): Printer[String, Char, Char, Char] =
    regexChar(Regex.charIn(chars: _*), s"Not the expected character (${chars.mkString(", ")})")

  /** Printer that prints a single character except if it matches any of the given 'chars' */
  def charNotIn(chars: Char*): Printer[String, Char, Char, Char] =
    regexChar(Regex.charNotIn(chars: _*), s"One of the excluded characters (${chars.mkString(", ")})")

  /** Printer that just prints the input string */
  val anyString: Printer[Nothing, Char, String, String] =
    unsafeRegex(Regex.anyChar.atLeast(0))
      .transform(s => String.valueOf(s.toArray), (s: String) => Chunk.fromArray(s.toCharArray))

  /** Prints a specific string 'str' and results in 'value' */
  def string[Result](str: String, value: Result): Printer[Nothing, Char, Result, Result] =
    regexDiscard(Regex.string(str), Chunk.fromArray(str.toCharArray)).asPrinted(value, ())

  /** Printer that does not print anything and results in unit */
  def unit: Printer[Nothing, Nothing, Any, Unit] = succeed(())

  /** Prints a single alpha-numeric character */
  val alphaNumeric: Printer[String, Char, Char, Char] = regexChar(Regex.anyAlphaNumeric, "not alphanumeric")

  /** Prints a single digit */
  val digit: Printer[String, Char, Char, Char] = regexChar(Regex.anyDigit, "not a digit")

  /** Prints a single letter */
  val letter: Printer[String, Char, Char, Char] = regexChar(Regex.anyLetter, "not a letter")

  /** Prints a single whitespace character */
  val whitespace: Printer[String, Char, Char, Char] = regexChar(Regex.whitespace, "not a whitespace")
}
