package zio.parser

/** Methods of [[Parser]] that exist only on some Scala versions: on Scala 3 it adds `orElseU`, on Scala 2 it adds
  * nothing.
  */
trait VersionSpecificParser[+Err, -In, +Result] {
  self: Parser[Err, In, Result] =>

  /** Assigns 'that' parser as a fallback of this. First this parser gets evaluated. In case it succeeds, the result is
    * this parser's result. In case it fails, the result is 'that' parser's result. Unlike `orElse`, the two results can
    * have unrelated types: the result type is the union `Result | Result2`.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
    *
    * {{{
    * val letterOrNumber: Parser[String, Char, Char | Int] = Parser.letter.orElseU(Parser.digit.map(_.asDigit))
    * letterOrNumber.parseString("7") // Right(7)
    * }}}
    */
  final def orElseU[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result | Result2] =
    Parser.OrElse(Parser.Lazy(() => self), Parser.Lazy(() => that))

}
