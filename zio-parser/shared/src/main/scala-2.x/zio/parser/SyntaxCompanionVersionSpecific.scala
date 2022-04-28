package zio.parser

trait SyntaxCompanionVersionSpecific {
  def oneOf[A, I, O](
    syntax: Syntax[String, I, O, _ <: A],
    syntaxes: Syntax[String, I, O, _ <: A]*,
  ): Syntax[String, I, O, A] = 
    syntaxes.map(_.widen[A]).foldLeft(syntax.widen[A])(_ | _)
}
