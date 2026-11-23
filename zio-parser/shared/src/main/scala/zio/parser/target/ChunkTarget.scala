package zio.parser.target

import zio.parser.internal.Stack
import zio.{Chunk, ChunkBuilder}

/** A [[Target]] that collects the output elements in memory. `Printer#print(value)` uses it, and returns `result`.
  *
  * `emit` and `drop` throw a `java.lang.IllegalStateException` when the capture is not the most recent open one.
  *
  * {{{
  * val target = new ChunkTarget[Int]
  * target.write(1)
  * val kept = target.capture()
  * target.write(2)
  * target.emit(kept)
  * val dropped = target.capture()
  * target.write(3)
  * target.drop(dropped)
  *
  * target.result // Chunk(1,2)
  * }}}
  */
class ChunkTarget[Output] extends Target[Output] {
  private val builder: ChunkBuilder[Output]        = ChunkBuilder.make()
  private val captureStack: Stack[Capture]         = Stack()
  private var currentBuilder: ChunkBuilder[Output] = builder

  override type Capture = ChunkTarget.Capture[Output]

  /** The output elements written so far, without the output of captures that are still open */
  def result: Chunk[Output] = builder.result()

  override def write(value: Output): Unit = {
    currentBuilder += value
    ()
  }

  override def capture(): Capture = {
    val capture = ChunkTarget.Capture(ChunkBuilder.make[Output]())
    captureStack.push(capture)
    currentBuilder = capture.subBuilder
    capture
  }

  override def emit(capture: Capture): Unit = {
    val popped = captureStack.pop()
    if (popped != capture) {
      throw new IllegalStateException(s"emit called on a capture group not on top of the stack")
    }
    if (!captureStack.isEmpty) {
      currentBuilder = captureStack.peek().subBuilder
    } else {
      currentBuilder = builder
    }
    currentBuilder ++= capture.subBuilder.result()
    ()
  }

  override def drop(capture: Capture): Unit = {
    val popped = captureStack.pop()
    if (popped != capture) {
      throw new IllegalStateException(s"emit called on a capture group not on top of the stack")
    }
    if (!captureStack.isEmpty) {
      currentBuilder = captureStack.peek().subBuilder
    } else {
      currentBuilder = builder
    }
  }
}

/** Holds the capture token type of [[ChunkTarget]]. */
object ChunkTarget {

  /** A capture group of a [[ChunkTarget]]: 'subBuilder' collects the output written while the capture is open */
  case class Capture[Output](subBuilder: ChunkBuilder[Output])
}
