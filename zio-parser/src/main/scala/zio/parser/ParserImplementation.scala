package zio.parser

/** The available parser implementations
  *
  * The default parser implementation is Recursive. There is an alternative implementation available which is stack-safe
  * but slower.
  */
sealed trait ParserImplementation
object ParserImplementation {
  case object StackSafe extends ParserImplementation
  case object Recursive extends ParserImplementation
}
