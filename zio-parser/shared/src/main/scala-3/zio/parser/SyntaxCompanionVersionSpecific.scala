package zio.parser

/** Constructors of the [[Syntax]] companion whose implementation differs between Scala versions: `oneOf`. */
trait SyntaxCompanionVersionSpecific {

  /** Syntax that combines the syntaxes of the subtypes of a sum type `A`, in the given order. Parsing tries each in
    * turn, like `|`, and results in the first success; when all fail, the error is a nested
    * [[Parser.ParserError.AllBranchesFailed]] (with the exception described for `Parser#orElse`). Printing tries the
    * syntaxes in the same order and uses the first that prints the value; a syntax whose value class (from its
    * `ClassTag`) does not match the value fails without printing.
    *
    * Currently, on Scala 2 every value passes the class check of the first syntax, so printing a value that only a
    * later syntax handles throws (on the JVM, a `java.lang.ClassCastException`), unless the earlier syntaxes fail on it
    * the way constant syntaxes such as `Syntax.string("red", Red)` do.
    *
    * {{{
    * sealed trait Color
    * case object Red  extends Color
    * case object Blue extends Color
    *
    * val color: Syntax[String, Char, Char, Color] =
    *   Syntax.oneOf(Syntax.string("red", Red), Syntax.string("blue", Blue))
    *
    * color.parseString("blue") // Right(Blue)
    * color.printString(Blue)   // Right(blue)
    * }}}
    */
  inline def oneOf[A, I, O](
      inline syntax: Syntax[String, I, O, _ <: A],
      inline syntaxes: Syntax[String, I, O, _ <: A]*
  ): Syntax[String, I, O, A] =
    ${ Macros.oneOfImpl[A, I, O]('syntax, 'syntaxes) }
}
