package zio.parser

object SyntaxCompanionOps {
  final implicit class SyntaxCompanionSyntax(val companion: Syntax.type) extends AnyVal {
    def oneOf[A, I, O](
      syntax: Syntax[String, I, O, _ <: A],
      syntaxes: Syntax[String, I, O, _ <: A]*,
    ): Syntax[String, I, O, A] =
      syntaxes.map(_.widen[A]).foldLeft(syntax.widen[A])(_ | _)
  }
}
