package zio.parser.target

/** The destination of a printer's output elements. `Printer#print(value, target)` and `Syntax#print(value, target)`
  * write each element to the target with `write`.
  *
  * When a printer tries alternatives (`orElse`, `|`), it calls `capture` before the first one, and then `emit` when it
  * succeeds or `drop` when it fails, so the output of a failed alternative is not kept. Captures nest: `emit` and
  * `drop` get the most recent capture that is still open.
  *
  * The library has two implementations: [[ChunkTarget]], which collects the output in memory, and [[ZStreamTarget]],
  * which offers it to a queue read by a `ZStream`.
  *
  * {{{
  * import zio.Chunk
  * import zio.parser._
  *
  * val target: Target[Char] = new ChunkTarget[Char]
  *
  * Syntax.letter.repeat.print(Chunk('a', 'b', 'c'), target) // Right(())
  * }}}
  *
  * @see
  *   [[https://zio.dev/zio-parser/printing/ Printing and Invertible Syntaxes]]
  */
trait Target[Output] {

  /** The token returned by `capture`, identifying one capture group */
  type Capture

  /** Writes one output element, into the most recent open capture if there is one */
  def write(value: Output): Unit

  /** Starts capturing the output written from now on, and returns the token for `emit` or `drop` */
  def capture(): Capture

  /** Ends 'capture' and keeps its output: it goes to the enclosing capture, or to the target when no other capture is
    * open
    */
  def emit(capture: Capture): Unit

  /** Ends 'capture' and discards the output written since it started */
  def drop(capture: Capture): Unit
}
