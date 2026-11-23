package zio.parser

/** The available parser implementations
  *
  * The default parser implementation is Recursive. There is an alternative implementation available which is stack-safe
  * but slower.
  */
sealed trait ParserImplementation

/** The two [[ParserImplementation]] values. */
object ParserImplementation {

  /** Runs the parser as a loop over operations compiled from the parser tree (`Parser#compiledOpStack`) instead of
    * recursive calls.
    *
    * Currently it differs from [[ParserImplementation.Recursive]] in a few cases: `exactly` does not stop after the
    * given number of items unless the item parser is built with `regexChar`, `mapError` throws a
    * `java.lang.NullPointerException` when the parser succeeds, `Parser.any` throws a `scala.NotImplementedError`, the
    * unsafe regex parsers succeed with an empty `Chunk` when their regex does not match, and too few repetitions fail
    * with [[Parser.ParserError.UnexpectedEndOfInput]].
    */
  case object StackSafe extends ParserImplementation

  /** The default implementation, in which each node of the optimized parser tree calls the nodes below it. */
  case object Recursive extends ParserImplementation
}
