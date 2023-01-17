package zio.parser.internal.recursive

import zio.Chunk
import zio.parser.Parser.ParserError
import zio.parser.Regex

/** State of the recursive parser implementation
  *
  * The implementation itself is in Parser#parseRec.
  */
sealed trait ParserState[+In] {

  def regex(compiledRegex: Regex.Compiled): Int
  def sliceToChunk(pos: Int, until: Int): Chunk[In]
  def sliceToString(pos: Int, until: Int): String
  def at(pos: Int): In
  val length: Int

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

object ParserState {
  private final class StringParserState(source: String) extends ParserState[Char] {
    override def regex(compiledRegex: Regex.Compiled): Int =
      compiledRegex.test(position, source)

    override def sliceToChunk(pos: Int, until: Int): Chunk[Char] =
      Chunk.fromArray(source.slice(pos, until).toCharArray)


    override def sliceToString(pos: Int, until: Int): String =
      source.slice(pos, until)

    override def at(pos: Int): Char = source(pos)

    override val length: Int = source.length
  }

  private final class CharChunkParserState(source: Chunk[Char]) extends ParserState[Char] {
    override def regex(compiledRegex: Regex.Compiled): Int =
      compiledRegex.test(position, source)

    override def sliceToChunk(pos: Int, until: Int): Chunk[Char] =
      source.slice(pos, until)

    override def sliceToString(pos: Int, until: Int): String =
      new String(source.slice(pos, until).toArray)

    override def at(pos: Int): Char = source(pos)

    override val length: Int = source.length
  }

  private final class ChunkParserState[In](source: Chunk[In]) extends ParserState[In] {
    override def regex(compiledRegex: Regex.Compiled): Int =
      throw new UnsupportedOperationException("regex not supported on non-char Chunks")

    override def sliceToChunk(pos: Int, until: Int): Chunk[In] =
      source.slice(pos, until)

    override def sliceToString(pos: Int, until: Int): String =
      throw new UnsupportedOperationException("sliceToString not supported on non-char Chunks")

    override def at(pos: Int): In = source(pos)

    override val length: Int = source.length
  }

  def fromString(source: String): ParserState[Char] =
    new StringParserState(source)

  def fromChunk[In](chunk: Chunk[In])(implicit stateSelector: StateSelector[In]): ParserState[In] =
    stateSelector.create(chunk)

  trait StateSelector[In] {
    def create(chunk: Chunk[In]): ParserState[In]
  }

  object StateSelector extends LowerPriorityStateSelector {
    implicit val charStateSelector: StateSelector[Char] = (chunk: Chunk[Char]) => new CharChunkParserState(chunk)
  }

  trait LowerPriorityStateSelector {
    implicit def otherStateSelector[In]: StateSelector[In] = (chunk: Chunk[In]) => new ChunkParserState(chunk)
  }
}
