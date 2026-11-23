package zio

import zio.parser.internal.{PUnzippable, PZippable}

import scala.reflect.ClassTag
import scala.util.Try

package object parser extends ImplicitTupleConversion {

  /** Adds `~>` and `zipRight` to every [[Parser]]. */
  implicit final class AnyParserOps[Err, In](private val self: Parser[Err, In, Any]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Err2 >: Err, In2 <: In, Result](
        that: => Parser[Err2, In2, Result]
    ): Parser[Err2, In2, Result] =
      zipRight(that)

    /** Concatenates this parser with that parser, and if both succeeds, discard the first result and use the second.
      */
    def zipRight[Err2 >: Err, In2 <: In, Result](
        that: => Parser[Err2, In2, Result]
    ): Parser[Err2, In2, Result] =
      Parser.ZipRight(Parser.Lazy(() => self), Parser.Lazy(() => that))
  }

  /** Adds `to`, which converts a parser's tuple result to a case class. */
  implicit final class TupleParserOps[Err, In, Result](private val self: Parser[Err, In, Result]) extends AnyVal {

    /** Maps the parser's successful tuple result to the given case class */
    def to[Result2 <: Product](implicit conversion: TupleConversion[Result2, Result]): Parser[Err, In, Result2] =
      self.map(conversion.from)
  }

  /** Adds `~>`, `zipRight` and `as` to syntaxes whose value is unit. */
  implicit final class AnySyntaxOps[Err, In, Out](private val self: Syntax[Err, In, Out, Unit]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Err2 >: Err, In2 <: In, Out2 >: Out, Value2](
        that: => Syntax[Err2, In2, Out2, Value2]
    ): Syntax[Err2, In2, Out2, Value2] =
      zipRight(that)

    /** Concatenates this parser with that parser, and if both succeeds, discard the first result and use the second.
      * The printer prints unit with this syntax, then the value with that syntax.
      */
    def zipRight[Err2 >: Err, In2 <: In, Out2 >: Out, Value2](
        that: => Syntax[Err2, In2, Out2, Value2]
    ): Syntax[Err2, In2, Out2, Value2] =
      Syntax.from(
        self.asParser ~> that.asParser,
        self.asPrinter ~> that.asPrinter
      )

    /** Ignores the result of the syntax and result in 'value' instead.
      *
      * The printer accepts only 'value' and prints this syntax for it. For any other value it currently fails with a
      * `Printer.Failed` value in the error channel instead of an `Err`.
      *
      * {{{
      * import zio.parser._
      *
      * val yes: Syntax[String, Char, Char, Boolean] = Syntax.char('y').as(true)
      *
      * yes.parseString("y")  // Right(true)
      * yes.printString(true) // Right(y)
      * }}}
      */
    def as[Value2](value: => Value2): Syntax[Err, In, Out, Value2] =
      Syntax.from(
        self.asParser.as(value),
        self.asPrinter.asPrinted(value, ())
      )
  }

  /** Adds `widen` to syntaxes whose error type is `String`. */
  implicit class StringErrSyntaxOps[In, Out, Value](private val self: Syntax[String, In, Out, Value]) extends AnyVal {

    /** Widens the parser to a supertype of its result
      *
      * This is useful in combination with the orElse (<>) operator. For example a JSON parser can be expressed by a
      * combination of parsers for the individual json type widened to Json.
      *
      * The printer accepts a value only when its runtime class is the class of this syntax's value type, and fails
      * with `Not a X`, where `X` is the simple name of that class, otherwise. For value types such as `Char` or `Int`
      * the class check currently fails for every value, so the printer always fails.
      *
      * {{{
      * import zio.parser._
      *
      * sealed trait Json
      * case object JNull                  extends Json
      * final case class JBool(b: Boolean) extends Json
      *
      * val nul: Syntax[String, Char, Char, JNull.type] = Syntax.string("null", JNull)
      * val tru: Syntax[String, Char, Char, JBool]      = Syntax.string("true", JBool(true))
      *
      * val json: Syntax[String, Char, Char, Json] = nul.widen[Json] <> tru.widen[Json]
      *
      * json.parseString("null")           // Right(JNull)
      * json.printString(JBool(true))      // Right(true)
      * tru.widen[Json].printString(JNull) // Left(Not a JBool)
      * }}}
      */
    def widen[D](implicit ev: Value <:< D, tag: ClassTag[Value]): Syntax[String, In, Out, D] =
      self.asParser.asInstanceOf[Parser[String, In, D]] <=> self.asPrinter.contramapEither((value: D) =>
        if (tag.runtimeClass.isAssignableFrom(value.getClass)) {
          Right(value.asInstanceOf[Value])
        } else {
          val name = Try(tag.runtimeClass.getSimpleName).fold(error => error.getMessage, identity[String])
          Left(s"Not a $name")
        }
      )
  }

  /** Adds `~`, `zip`, `of` and `widenWith` to every [[Syntax]]. */
  implicit class SyntaxOps[Err, In, Out, Value](private val self: Syntax[Err, In, Out, Value]) {

    /** Symbolic alias for zip */
    final def ~[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, ZippedValue](
        that: => Syntax[Err2, In2, Out2, Value2]
    )(implicit
        unzippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Value, Value2, ZippedValue]
    ): Syntax[Err2, In2, Out2, ZippedValue] =
      zip(that)

    /** Concatenates this syntax with 'that' syntax. In case both parser succeeds, the result is a pair of the results.
      * The printer destructures a pair and prints the left value with this, the right value with 'that'.
      */
    final def zip[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, ZippedValue](
        that: => Syntax[Err2, In2, Out2, Value2]
    )(implicit
        unzippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Value, Value2, ZippedValue]
    ): Syntax[Err2, In2, Out2, ZippedValue] =
      Syntax.from(
        self.asParser.zip(that.asParser),
        self.asPrinter.zip(that.asPrinter)
      )

    /** Transforms the syntax of a tuple to a syntax of a given case class */
    def of[Value2 <: Product](implicit
        conversion: TupleConversion[Value2, Value],
        ev: Value =:= Value
    ): Syntax[Err, In, Out, Value2] =
      self.transform(conversion.from, conversion.to)

    /** Widens the value type to `D`. The printer accepts a value only when its runtime class is the class of this
      * syntax's value type, and fails with 'err' otherwise; for value types such as `Char` or `Int` this check
      * currently fails for every value.
      *
      * `Syntax` has its own `widenWith(narrow, failure)`, which hides this one, so it is called as
      * `SyntaxOps(syntax).widenWith[D](err)`.
      */
    def widenWith[D](err: Err)(implicit ev: Value <:< D, tag: ClassTag[Value]): Syntax[Err, In, Out, D] =
      self.asParser.asInstanceOf[Parser[Err, In, D]] <=> self.asPrinter.contramapEither((value: D) =>
        if (tag.runtimeClass.isAssignableFrom(value.getClass)) {
          Right(value.asInstanceOf[Value])
        } else {
          Left(err)
        }
      )
  }

  /** Adds `<=>`, which pairs a [[Parser]] with a [[Printer]] into a [[Syntax]]. */
  implicit class ParserOps[Err, In, Value](private val self: Parser[Err, In, Value]) extends AnyVal {

    /** Combines this parser with that printer to get a syntax.
      *
      * This operation enables the use of parser or printer-specific operators to build up fragments of a syntax. The
      * resulting syntax can be used as a building block to create bigger syntaxes.
      */
    def <=>[Out](that: Printer[Err, Out, Value]): Syntax[Err, In, Out, Value] =
      Syntax.from[Err, In, Out, Value](self, that)
  }

  /** Adds `~>` and `zipRight` to printers whose value is unit. */
  implicit class UnitPrinterOps[Err, Out](private val self: Printer[Err, Out, Unit]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Err2 >: Err, Out2 >: Out, Value](
        that: => Printer[Err2, Out2, Value]
    ): Printer[Err2, Out2, Value] =
      zipRight(that)

    /** Print Unit with this, then print that and use the second printer's result value
      */
    def zipRight[Err2 >: Err, Out2 >: Out, Value](
        that: => Printer[Err2, Out2, Value]
    ): Printer[Err2, Out2, Value] =
      Printer.ZipRight(Printer.Lazy(() => self), Printer.Lazy(() => that))
  }

  /** Adds `~>`, `zipRight`, `~` and `zip` to every [[Printer]]. */
  implicit class PrinterOps[Err, Out, Value](private val self: Printer[Err, Out, Value]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>(that: => Printer[Err, Out, Value]): Printer[Err, Out, Value] =
      zipRight(that)

    /** Print this, then print that. That printer gets the value to be printed; this printer currently gets unit
      * instead of the value, so use it with a printer that ignores its input.
      */
    def zipRight(that: => Printer[Err, Out, Value]): Printer[Err, Out, Value] =
      Printer.ZipRight(Printer.Lazy(() => self), Printer.Lazy(() => that))

    /** Symbolic alias for zip */
    final def ~[Err2 >: Err, Out2 >: Out, Value2, ZippedValue](
        that: => Printer[Err2, Out2, Value2]
    )(implicit zippableValue: PUnzippable.In[Value, Value2, ZippedValue]): Printer[Err2, Out2, ZippedValue] =
      zip(that)

    /** Take a pair to be printed, print the left value with this, and the right value with 'that'. The result is a pair
      * of both printer's results.
      */
    final def zip[Err2 >: Err, Out2 >: Out, Value2, ZippedValue](
        that: => Printer[Err2, Out2, Value2]
    )(implicit unzippableValue: PUnzippable.In[Value, Value2, ZippedValue]): Printer[Err2, Out2, ZippedValue] =
      Printer.Zip(Printer.Lazy(() => self), Printer.Lazy(() => that), unzippableValue.unzip)
  }

  /** Adds `from`, which prints a case class with a printer of the matching tuple. */
  implicit class TuplePrinterOps[Err, Out, Value <: Product](
      private val self: Printer[Err, Out, Value]
  ) {

    /** Transforms the printer's input from a given case class for a tuple printer */
    def from[Value2](implicit
        conversion: TupleConversion[Value2, Value]
    ): Printer[Err, Out, Value2] =
      self.contramap(conversion.to)
  }
}
