package zio.parser.target

import zio.parser.internal.Stack
import zio.stream.ZStream
import zio.{ChunkBuilder, Queue, Runtime, UIO, Unsafe, ZIO}

/** A [[Target]] that offers the output elements to an unbounded queue, which `stream` reads.
  *
  * Each `write` outside a capture, and each `emit` of the outermost capture, runs the queue offer on 'runtime' and
  * waits for it. The output of dropped captures never reaches the queue. Nothing closes the queue when printing
  * finishes, so `stream` does not end by itself: read it with `take` or a similar bound.
  *
  * Each run of `stream` takes elements from the same queue, so elements one run reads are gone for the next, and a run
  * that stops early, such as `take(2)`, can remove more elements than it returns.
  *
  * {{{
  * import zio._
  * import zio.parser._
  *
  * val program: UIO[String] =
  *   for {
  *     target <- ZStreamTarget[Char]
  *     _      <- ZIO.succeed(Syntax.anyString.print("42", target))
  *     out    <- target.stream.take(2).runCollect
  *   } yield out.mkString
  * // program succeeds with "42"
  * }}}
  *
  * @param runtime
  *   the runtime used to create the queue and to offer elements to it
  * @see
  *   [[https://zio.dev/zio-parser/printing/ Printing and Invertible Syntaxes]]
  */
class ZStreamTarget[R, Output](runtime: Runtime[R]) extends Target[Output] {
  private val queue: Queue[Output] = Unsafe.unsafe { implicit u =>
    runtime.unsafe.run(Queue.unbounded[Output]).getOrThrowFiberFailure()
  }

  /** A stream of the elements offered to the queue. It does not end by itself. */
  val stream: ZStream[Any, Nothing, Output] = ZStream.fromQueue(queue)

  private val captureStack: Stack[Capture]         = Stack()
  private var currentBuilder: ChunkBuilder[Output] = _

  override type Capture = ZStreamTarget.Capture[Output]

  override def write(value: Output): Unit =
    if (currentBuilder == null) {
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run(queue.offer(value).unit).getOrThrowFiberFailure()
      }
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
      Unsafe.unsafe { implicit u =>
        runtime.unsafe.run(queue.offerAll(capture.subBuilder.result()).unit).getOrThrowFiberFailure()
      }
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

/** Creates [[ZStreamTarget]] values. */
object ZStreamTarget {

  /** A capture group of a [[ZStreamTarget]]: 'subBuilder' collects the output written while the capture is open */
  case class Capture[Output](subBuilder: ChunkBuilder[Output])

  /** Creates a target that uses the runtime of the fiber running this effect */
  def apply[Output]: UIO[ZStreamTarget[Any, Output]] =
    ZIO.runtime[Any].map { runtime =>
      new ZStreamTarget[Any, Output](runtime)
    }
}
