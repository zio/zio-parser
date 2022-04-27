package zio.parser

object SyntaxCompanionOps {
  extension (syntaxCompanion: Syntax.type) {
    /** Syntax that combines all the constructors of subclasses of a sum type */
    inline def oneOf[A, I, O](
      inline syntax: Syntax[String, I, O, _ <: A],
      inline syntaxes: Syntax[String, I, O, _ <: A]*,
    ): Syntax[String, I, O, A] =
      ${ Macros.oneOfImpl[A, I, O]('syntax, 'syntaxes) }
  }

}
