package zio.parser

import scala.reflect.ClassTag

/** Members of [[Syntax]] that exist only on some Scala versions. On Scala 3 it adds `orElseU`; on Scala 2 it has no
  * members.
  */
trait VersionSpecificSyntax[+Err, -In, +Out, Value] { self: Syntax[Err, In, Out, Value] =>

  /** Assigns 'that' syntax as a fallback of this, with the union of the two value types as the value type. First this
    * parser gets evaluated. In case it succeeds, the result is this syntax's result. In case it fails, the result is
    * 'that' syntax's result.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
    *
    * The printer chooses the side by the runtime class of the value: a value of class `Value2` is printed with 'that',
    * any other value with this syntax, without a fallback when that side fails.
    *
    * {{{
    * // scala-3-only
    * val number: Syntax[String, Char, Char, Int]  = Syntax.digit.repeat.string.transform(_.toInt, _.toString)
    * val word: Syntax[String, Char, Char, String] = Syntax.letter.repeat.string
    *
    * val token: Syntax[String, Char, Char, Int | String] = number.orElseU(word)
    *
    * token.parseString("42")  // Right(42)
    * token.parseString("abc") // Right(abc)
    * token.printString(7)     // Right(7)
    * }}}
    */
  final def orElseU[Err2 >: Err, In2 <: In, Out2 >: Out, Value2: ClassTag](
      that: => Syntax[Err2, In2, Out2, Value2]
  )(implicit vtag: ClassTag[Value]): Syntax[Err2, In2, Out2, Value | Value2] =
    asParser.orElseU(that.asParser) <=> asPrinter.orElseU(that.asPrinter)
}
