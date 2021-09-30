package zio

import scala.reflect.ClassTag
import zio.parser.internal.{PZippable, PUnzippable}

package object parser {

  /** The available parser implementations
    *
    * The default parser implementation is Recursive. There is an alternative implementation available which is
    * stack-safe but slower.
    */
  sealed trait ParserImplementation
  object ParserImplementation {
    case object StackSafe extends ParserImplementation
    case object Recursive extends ParserImplementation
  }

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

  implicit final class UnitSyntaxOps[Err, In, Out](private val self: Syntax[Err, In, Out, Unit, Any]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, Result2](
        that: => Syntax[Err2, In2, Out2, Value2, Result2]
    ): Syntax[Err2, In2, Out2, Value2, Result2] =
      zipRight(that)

    /** Concatenates this parser with that parser, and if both succeeds, discard the first result and use the second.
      */
    def zipRight[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, Result2](
        that: => Syntax[Err2, In2, Out2, Value2, Result2]
    ): Syntax[Err2, In2, Out2, Value2, Result2] =
      Syntax.from(
        self.asParser ~> that.asParser,
        self.asPrinter ~> that.asPrinter
      )

    /** Ignores the result of the syntax and result in 'value' instead */
    def as[Result2](value: => Result2): Syntax[Err, In, Out, Result2, Result2] =
      Syntax.from(
        self.asParser.as(value),
        self.asPrinter.asPrinted(value, ())
      )
  }

  implicit class StringErrSyntaxOps[In, Out, Value, Result](private val self: Syntax[String, In, Out, Value, Result])
      extends AnyVal {

    /** Widens the parser to a supertype of its result
      *
      * This is useful in combination with the orElse (<>) operator. For example a JSON parser can be expressed by a
      * combination of parsers for the individual json type widened to Json:
      * {{{
      * nul.widen[Json] <>
      * bool.widen[Json] <>
      * str.widen[Json] <>
      * num.widen[Json] <>
      * list.widen[Json] <>
      * obj.widen[Json]
      * }}}
      */
    def widen[D](implicit ev: Result <:< D, tag: ClassTag[Value]): Syntax[String, In, Out, D, D] =
      self.asParser.asInstanceOf[Parser[String, In, D]] <=> self.asPrinter.transformEither(
        (value: Result) => Right(ev(value)),
        (value: D) =>
          if (tag.runtimeClass.isAssignableFrom(value.getClass)) {
            Right(value.asInstanceOf[Value])
          } else {
            Left(s"Not a ${tag.runtimeClass.getSimpleName}")
          }
      )
  }

  implicit class SyntaxOps[Err, In, Out, Value, Result](private val self: Syntax[Err, In, Out, Value, Result]) {

    /** Symbolic alias for zip */
    final def ~[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, Result2, ZippedValue, ZippedResult](
        that: => Syntax[Err2, In2, Out2, Value2, Result2]
    )(implicit
        unzippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Result, Result2, ZippedResult]
    ): Syntax[Err2, In2, Out2, ZippedValue, ZippedResult] =
      zip(that)

    /** Concatenates this syntax with 'that' syntax. In case both parser succeeds, the result is a pair of the results.
      * The printer destructures a pair and prints the left value with this, the right value with 'that'.
      */
    final def zip[Err2 >: Err, In2 <: In, Out2 >: Out, Value2, Result2, ZippedValue, ZippedResult](
        that: => Syntax[Err2, In2, Out2, Value2, Result2]
    )(implicit
        unzippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Result, Result2, ZippedResult]
    ): Syntax[Err2, In2, Out2, ZippedValue, ZippedResult] =
      Syntax.from(
        self.asParser.zip(that.asParser),
        self.asPrinter.zip(that.asPrinter)
      )
  }

  implicit class ParserOps[Err, In, Result](private val self: Parser[Err, In, Result]) extends AnyVal {

    /** Combines this parser with that printer to get a syntax.
      *
      * This operation enables the use of parser or printer-specific operators to build up fragments of a syntax. The
      * resulting syntax can be used as a building block to create bigger syntaxes.
      */
    def <=>[Out, Value](that: Printer[Err, Out, Value, Result]): Syntax[Err, In, Out, Value, Result] =
      Syntax.from[Err, In, Out, Value, Result](self, that)
  }

  implicit class UnitPrinterOps[Err, Out](private val self: Printer[Err, Out, Unit, Any]) extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Err2 >: Err, Out2 >: Out, Value, Result](
        that: => Printer[Err2, Out2, Value, Result]
    ): Printer[Err2, Out2, Value, Result] =
      zipRight(that)

    /** Print Unit with this, then print that and use the second printer's result value
      */
    def zipRight[Err2 >: Err, Out2 >: Out, Value, Result](
        that: => Printer[Err2, Out2, Value, Result]
    ): Printer[Err2, Out2, Value, Result] =
      Printer.ZipRight(Printer.Lazy(() => self), Printer.Lazy(() => that))
  }

  implicit class PrinterOps[Err, Out, Value, Result](private val self: Printer[Err, Out, Value, Result])
      extends AnyVal {

    /** Symbolic alias for zipRight
      */
    def ~>[Result2](that: => Printer[Err, Out, Value, Result2]): Printer[Err, Out, Value, Result2] =
      zipRight(that)

    /** Print this, then print that and use the second printer's result value. Both printers get the same value to be
      * printed.
      */
    def zipRight[Result2](that: => Printer[Err, Out, Value, Result2]): Printer[Err, Out, Value, Result2] =
      Printer.ZipRight(Printer.Lazy(() => self), Printer.Lazy(() => that))

    /** Symbolic alias for zip */
    final def ~[Err2 >: Err, Out2 >: Out, Value2, Result2, ZippedValue, ZippedResult](
        that: => Printer[Err2, Out2, Value2, Result2]
    )(implicit
        zippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Result, Result2, ZippedResult]
    ): Printer[Err2, Out2, ZippedValue, ZippedResult] =
      zip(that)

    /** Take a pair to be printed, print the left value with this, and the right value with 'that'. The result is a pair
      * of both printer's results.
      */
    final def zip[Err2 >: Err, Out2 >: Out, Value2, Result2, ZippedValue, ZippedResult](
        that: => Printer[Err2, Out2, Value2, Result2]
    )(implicit
        unzippableValue: PUnzippable.In[Value, Value2, ZippedValue],
        zippableResult: PZippable.Out[Result, Result2, ZippedResult]
    ): Printer[Err2, Out2, ZippedValue, ZippedResult] =
      Printer.Zip(Printer.Lazy(() => self), Printer.Lazy(() => that), unzippableValue.unzip, zippableResult.zip)

  }
}
