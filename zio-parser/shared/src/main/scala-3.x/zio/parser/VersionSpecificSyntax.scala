package zio.parser

import scala.reflect.ClassTag

trait VersionSpecificSyntax[+Err, -In, +Out, Value] { self: Syntax[Err, In, Out, Value] =>

  /** Assigns 'that' syntax as a fallback of this. First this parser or printer gets evaluated. In case it succeeds, the
    * result is this syntax's result. In case it fails, the result is 'that' syntax's result.
    *
    * If auto-backtracking is on, this parser will backtrack before trying 'that' parser.
    */
  final def orElseU[Err2 >: Err, In2 <: In, Out2 >: Out, Value2: ClassTag](
      that: => Syntax[Err2, In2, Out2, Value2]
  ): Syntax[Err2, In2, Out2, Value | Value2] =
    asParser.orElseU(that.asParser) <=> asPrinter.orElseU(that.asPrinter)
}
