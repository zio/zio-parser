package zio.parser.internal.recursive

import zio.parser.Parser.ParserError

/** State of the recursive parser implementation
  *
  * The implementation itself is in Parser#parseRec.
  *
  * @param source
  *   The parsed string
  */
final class ParserState(val source: String) {

  /** Position in the parsed string (source) */
  var position: Int = 0

  /** Stack of named parsers, used to enrich failures */
  var nameStack: List[String] = Nil

  /** Error emitted during parsing */
  var error: ParserError[Any] = _

  /** Flag indicating that parser results are not being used */
  var discard: Boolean = false

  /** Push a name to nameStack */
  def pushName(name: String): Unit =
    nameStack = name :: nameStack

  /** Pop the last pushed name from nameStack */
  def popName(): Unit =
    nameStack = nameStack.tail
}
