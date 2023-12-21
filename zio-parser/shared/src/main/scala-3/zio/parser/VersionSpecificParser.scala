package zio.parser

trait VersionSpecificParser[+Err, -In, +Result] {
  self: Parser[Err, In, Result] =>

  /** Assigns 'that' parser as a fallback of this. First this parser gets evaluated. In case it succeeds, the result is
    * this parser's result. In case it fails, the result is 'that' parser's result.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
    */
  final def orElseU[Err2 >: Err, In2 <: In, Result2](
      that: => Parser[Err2, In2, Result2]
  ): Parser[Err2, In2, Result | Result2] =
    Parser.OrElse(Parser.Lazy(() => self), Parser.Lazy(() => that))

}
