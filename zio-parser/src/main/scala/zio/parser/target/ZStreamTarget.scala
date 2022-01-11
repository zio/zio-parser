package zio.parser.target

import zio.parser.internal.Stack
import zio.stream.ZStream
import zio.{ChunkBuilder, Queue, Runtime, UIO, ZIO, ZQueue}

class ZStreamTarget[R, Output](runtime: Runtime[R]) extends Target[Output] {
  private val queue: Queue[Output] = runtime.unsafeRun(ZQueue.unbounded[Output])

  val stream: ZStream[Any, Nothing, Output] = ZStream.fromQueue(queue)

  private val captureStack: Stack[Capture]         = Stack()
  private var currentBuilder: ChunkBuilder[Output] = _

  override type Capture = ZStreamTarget.Capture[Output]

  override def write(value: Output): Unit =
    if (currentBuilder == null) {
      runtime.unsafeRun(queue.offer(value).unit)
    } else {
      currentBuilder += value
      ()
    }

  override def capture(): Capture = {
    val capture = ZStreamTarget.Capture(ChunkBuilder.make[Output]())
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
      currentBuilder ++= capture.subBuilder.result()
      ()
    } else {
      currentBuilder = null
      runtime.unsafeRun(queue.offerAll(capture.subBuilder.result()).unit)
    }
  }

  override def drop(capture: Capture): Unit = {
    val popped = captureStack.pop()
    if (popped != capture) {
      throw new IllegalStateException(s"emit called on a capture group not on top of the stack")
    }
    if (!captureStack.isEmpty) {
      currentBuilder = captureStack.peek().subBuilder
    } else {
      currentBuilder = null
    }
  }
}

object ZStreamTarget {
  case class Capture[Output](subBuilder: ChunkBuilder[Output])

  def apply[Output]: UIO[ZStreamTarget[Any, Output]] =
    ZIO.runtime[Any].map { runtime =>
      new ZStreamTarget[Any, Output](runtime)
    }
}
