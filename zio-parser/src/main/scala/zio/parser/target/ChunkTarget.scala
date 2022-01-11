package zio.parser.target

import zio.parser.internal.Stack
import zio.{Chunk, ChunkBuilder}

class ChunkTarget[Output] extends Target[Output] {
  private val builder: ChunkBuilder[Output]        = ChunkBuilder.make()
  private val captureStack: Stack[Capture]         = Stack()
  private var currentBuilder: ChunkBuilder[Output] = builder

  override type Capture = ChunkTarget.Capture[Output]

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

object ChunkTarget {
  case class Capture[Output](subBuilder: ChunkBuilder[Output])
}
