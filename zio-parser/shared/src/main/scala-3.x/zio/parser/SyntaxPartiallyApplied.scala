package zio.parser

final class SyntaxPartiallyApplied[A] {
  inline def apply[I, O](
      inline syntax: Syntax[String, I, O, _ <: A],
      inline syntaxes: Syntax[String, I, O, _ <: A]*
  ): Syntax[String, I, O, A] =
    ${ Macros.oneOfImpl[A, I, O]('syntax, 'syntaxes) }
}
