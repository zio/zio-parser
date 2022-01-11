package zio.parser.internal.stacksafe

import zio.Chunk
import zio.parser.Parser.{ErasedParser, ParserError}
import zio.parser.internal.Stack
import zio.parser.{Parser, Regex}

import scala.annotation.tailrec
import scala.collection.mutable

/** Parser operation, the language Parser is precompiled to for parsing */
sealed trait ParserOp {
  val needsEmptyResultSlot: Boolean
}
object ParserOp       {

  /** Push a and then b to the operation stack */
  final case class PushOp2(a: ParserOp, b: ParserOp, pushBranchPosition: Boolean) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Push a, b and then c to the operation stack */
  final case class PushOp3(a: ParserOp, b: ParserOp, c: ParserOp) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Push a, b, c and then d to the operation stack */
  final case class PushOp4(a: ParserOp, b: ParserOp, c: ParserOp, d: ParserOp, pushBranchPosition: Boolean)
      extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Deferred parser operation, used for recursion */
  final case class Lazy(op: () => ParserOp) extends ParserOp {
    lazy val memoized: ParserOp                = op()
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Store a result. One of success or failure must be null. If popFirst is true, the last result will be replaced but
    * only if it was a success.
    */
  final case class PushResult(success: AnyRef, failure: ParserError[Any], popFirst: Boolean) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = !popFirst
  }

  /** Pops the last stored branch position and the last result, and replaces it with the captured string as a result.
    */
  final case class PushCapturedResult() extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pushes the current input position as a result */
  final case class PushCurrentPosition() extends ParserOp {
    override val needsEmptyResultSlot: Boolean = true
  }

  /** Pushes a failure if the current position is not at the end */
  final case class CheckEnd() extends ParserOp {
    override val needsEmptyResultSlot: Boolean = true
  }

  /** Store a name in the name stack */
  final case class PushName(name: String) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last pushed name from the name stack */
  case object PopName extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Read an item from the input and push it to the result stack */
  case object ReadInputToResult extends ParserOp {
    override val needsEmptyResultSlot: Boolean = true
  }

  /** Match a sequence on the input, and push the given value or failure on the result stack */
  final case class MatchSeq(sequence: Chunk[Any], as: AnyRef, createParserFailure: (Int, Any) => Any) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = true
  }

  /** Match a compiled regex on the input, and push a result with the given strategy or failure on the result stack */
  final case class MatchRegex(regex: Regex.Compiled, pushAs: RegexResultPush, failAs: Option[Any]) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = true
  }

  /** Pop the last result from the stack, transform it to either a success or a failure and push back */
  final case class TransformResultEither(f: Any => Either[Any, Any]) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last result from the stack, transform it and with one of the functions and push back. It is possible to
    * pass null to onSuccess or onFailure in which case it does not touch the result.
    */
  final case class TransformResult(onSuccess: Any => Any, onFailure: ParserError[Any] => ParserError[Any])
      extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last result from the stack, transform it and with one of the functions and push back. It converts success
    * to failure and failure to success.
    */
  final case class TransformResultFlipped(
      onSuccess: (Int, Any) => ParserError[Any],
      onFailure: (Int, ParserError[Any]) => Any
  ) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last two results from the stack and if both were success, create a single value based on the given
    * strategy and push it back. If any of them failed, push back a single failure.
    */
  final case class TransformLast2Results(strategy: PairTransformation) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last result from the stack and if it was success, respush wrapped in Some, if it was failure, repush as a
    * successful None. When checkBranchPosition is true, if position was moved compared to the last branch position then
    * keep the failure.
    */
  final case class TransformResultToOption(checkBranchPosition: Boolean) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last result and use the function to push it as parser operation (flatmap) */
  case class PopResultPushOp(f: Any => ParserOp) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** If the result is failure, skip the next two parser operations. This can be used to shortcut the right side of a
    * zip operation.
    */
  case object SkipOnFailure2 extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** If the result is success, skip the next two parser operations. Optionally if the transform is not null, it
    * replaces the result with the transform function applied to it. This can be used to shortcut the right side of an
    * or operation. If checkBranchPosition is true, the last branch position will be popped and checked and if the
    * position was moved the left failure is kept and the next operations get skipped.
    */
  final case class SkipOnSuccess2(checkBranchPosition: Boolean, transform: Any => Any) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Creates a chunk builder and pushes on the chunk builder stack */
  final case class PushChunkBuilder(sizeHint: Int) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pushes the last result to the top chunk builder. If the last result was successful, repush the element parser and
    * itself. If the last result is a failure, finish building the result and check the min/max constraints.
    */
  final case class ProcessRepeatedElement(parseElement: ParserOp, min: Int, max: Option[Int]) extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Cut stored bookmarks if result is a success */
  case object Cut extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  /** Pop the last branch position, and in case of failure, reset the position to it. */
  case object BacktrackOnFailure extends ParserOp {
    override val needsEmptyResultSlot: Boolean = false
  }

  sealed trait RegexResultPush
  object RegexResultPush {
    case object MatchedChunk extends RegexResultPush
    case object SingleChar   extends RegexResultPush
    case object Ignored      extends RegexResultPush
  }

  sealed trait PairTransformation
  object PairTransformation {
    final case class Zip(zip: (Any, Any) => Any) extends PairTransformation
    case object KeepFirst                        extends PairTransformation
    case object KeepSecond                       extends PairTransformation
    case object IgnoreFirstKeepSecond            extends PairTransformation
    case object IgnoreFirstWrapSecondAsRight     extends PairTransformation
  }

  private final case class CompilerState(
      optimized: mutable.Map[ErasedParser, ParserOp],
      visited: mutable.Set[ErasedParser]
  )
  private object CompilerState {
    def initial: CompilerState =
      new CompilerState(mutable.Map.empty, mutable.Set.empty)
  }

  def compile(syntax: ErasedParser): InitialParser =
//    Debug.printParserTree(syntax)
    toInitialParser(
      compile(
        syntax,
        CompilerState.initial
      )
    )

  private def compile(syntax: ErasedParser, state: CompilerState): ParserOp =
    state.optimized.get(syntax) match {
      case Some(alreadyOptimized) =>
        alreadyOptimized
      case None                   =>
        state.visited.add(syntax)
        val compiled = compileParserNode(syntax, state)
        state.optimized.put(syntax, compiled)
        compiled
    }

  private def compileParserNode(parser: ErasedParser, state: CompilerState): ParserOp = {
    parser match {
      case l @ Parser.Lazy(_)         =>
        if (state.visited.contains(l.memoized.asInstanceOf[ErasedParser])) {
          Lazy(() => state.optimized(l.memoized.asInstanceOf[ErasedParser]))
        } else {
          compile(l.memoized.asInstanceOf[ErasedParser], state)
        }
      case Parser.Succeed(value)      =>
        PushResult(value.asInstanceOf[AnyRef], null, popFirst = false)
      case Parser.Fail(failure)       =>
        PushResult(null, ParserError.Failure(Nil, -1, failure), popFirst = false)
      case Parser.Failed(failure)     =>
        PushResult(null, failure, popFirst = false)
      case Parser.Named(syntax, name) =>
        PushOp3(PopName, compile(syntax, state), PushName(name))

      case sr @ Parser.SkipRegex(_, onFailure) =>
        MatchRegex(sr.compiledRegex, RegexResultPush.Ignored, onFailure)

      case pr @ Parser.ParseRegex(_, onFailure) =>
        MatchRegex(pr.compiledRegex, RegexResultPush.MatchedChunk, onFailure)

      case pr @ Parser.ParseRegexLastChar(_, onFailure) =>
        MatchRegex(pr.compiledRegex, RegexResultPush.SingleChar, onFailure)

      case Parser.TransformEither(syntax, to) =>
        PushOp2(
          TransformResultEither(to.asInstanceOf[Any => Either[Any, Any]]),
          compile(syntax, state),
          pushBranchPosition = false
        )

      case Parser.Transform(syntax, to) =>
        PushOp2(
          TransformResult(to.asInstanceOf[Any => Any], null),
          compile(syntax, state),
          pushBranchPosition = false
        )

      case Parser.Ignore(syntax, to) =>
        PushOp2(
          PushResult(to.asInstanceOf[AnyRef], null, popFirst = true),
          compile(syntax, state),
          pushBranchPosition = false
        )

      case Parser.CaptureString(syntax) =>
        PushOp2(
          PushCapturedResult(),
          compile(syntax, state),
          pushBranchPosition = true
        )

      case Parser.MapError(syntax, mapParserErr) =>
        PushOp2(
          TransformResult(null, mapParserErr.asInstanceOf[ParserError[Any] => ParserError[Any]]),
          compile(syntax, state),
          pushBranchPosition = false
        )

      case Parser.Zip(left, right, zip) =>
        val compiledLeft  = compile(left, state)
        val compiledRight = compile(right, state)
        PushOp4(
          TransformLast2Results(PairTransformation.Zip(zip.asInstanceOf[(Any, Any) => Any])),
          compiledRight,
          SkipOnFailure2,
          compiledLeft,
          pushBranchPosition = false
        )

      case Parser.ZipLeft(left, right) =>
        val compiledLeft  = compile(left, state)
        val compiledRight = compile(right, state)
        PushOp4(
          TransformLast2Results(PairTransformation.KeepFirst),
          compiledRight,
          SkipOnFailure2,
          compiledLeft,
          pushBranchPosition = false
        )

      case Parser.ZipRight(left, right) =>
        val compiledLeft  = compile(left, state)
        val compiledRight = compile(right, state)
        PushOp4(
          TransformLast2Results(PairTransformation.KeepSecond),
          compiledRight,
          SkipOnFailure2,
          compiledLeft,
          pushBranchPosition = false
        )

      case Parser.FlatMap(syntax, f) =>
        PushOp2(
          PopResultPushOp(f.asInstanceOf[Any => ErasedParser].andThen(compile(_, state))),
          compile(syntax, state),
          pushBranchPosition = false
        )

      case Parser.OrElseEither(left, right) =>
        PushOp4(
          TransformLast2Results(PairTransformation.IgnoreFirstWrapSecondAsRight),
          compile(right, state),
          SkipOnSuccess2(checkBranchPosition = true, Left(_)),
          compile(left, state),
          pushBranchPosition = true
        )

      case Parser.OrElse(left, right) =>
        PushOp4(
          TransformLast2Results(PairTransformation.IgnoreFirstKeepSecond),
          compile(right, state),
          SkipOnSuccess2(checkBranchPosition = true, null),
          compile(left, state),
          pushBranchPosition = true
        )

      case Parser.Optional(syntax) =>
        PushOp2(
          TransformResultToOption(checkBranchPosition = true),
          compile(syntax, state),
          pushBranchPosition = true
        )

      case r @ Parser.Repeat(syntax, min, max) =>
        val parseElement = compile(syntax, state)
        PushOp3(
          ProcessRepeatedElement(parseElement, min, max),
          parseElement,
          PushChunkBuilder(r.hint)
        )

      case Parser.Not(syntax, failure) =>
        val inner = compile(syntax, state)
        PushOp2(
          TransformResultFlipped((pos, _) => ParserError.Failure(Nil, pos, failure), (_, _) => ()),
          inner,
          pushBranchPosition = false
        )

      case Parser.Backtrack(syntax) =>
        PushOp2(BacktrackOnFailure, compile(syntax, state), pushBranchPosition = true)

      case Parser.SetAutoBacktrack(syntax, _) =>
        // This node is removed by the optimization phase so we can ignore it
        compile(syntax, state)

      case Parser.Index =>
        ParserOp.PushCurrentPosition()

      case Parser.End =>
        ParserOp.CheckEnd()
    }
  }

  private def toInitialParser(op: ParserOp): InitialParser = {
    @tailrec
    def collect(
        op: ParserOp,
        opStack: List[ParserOp],
        posCount: Int,
        names: List[String],
        builderHints: List[Int]
    ): InitialParser = {
      def finish(): InitialParser = {
        val ops = Stack[ParserOp]()
        opStack.reverse.foreach(ops.push)

        val pss = Array.fill(CharParserImpl.maxStoredPositions)(-1)
        for (i <- 0 until posCount) pss(i) = 0

        InitialParser(
          op,
          ops,
          pss,
          posCount,
          names,
          Chunk.fromIterable(builderHints.reverse)
        )
      }

      op match {
        case PushOp2(a, b, pushBranchPosition)       =>
          collect(b, a :: opStack, if (pushBranchPosition) posCount + 1 else posCount, names, builderHints)
        case PushOp3(a, b, c)                        =>
          collect(c, b :: a :: opStack, posCount, names, builderHints)
        case PushOp4(a, b, c, d, pushBranchPosition) =>
          collect(d, c :: b :: a :: opStack, if (pushBranchPosition) posCount + 1 else posCount, names, builderHints)
        case l @ Lazy(_)                             =>
          collect(l.memoized, opStack, posCount, names, builderHints)
        case PushResult(_, _, _)                     =>
          finish()
        case PushCapturedResult()                    =>
          finish()
        case PushName(name)                          =>
          collect(opStack.head, opStack.tail, posCount, name :: names, builderHints)
        case PopName                                 =>
          collect(opStack.head, opStack.tail, posCount, names.tail, builderHints)
        case ReadInputToResult                       =>
          finish()
        case MatchSeq(_, _, _)                       =>
          finish()
        case MatchRegex(_, _, _)                     =>
          finish()
        case TransformResultEither(_)                =>
          finish()
        case TransformResult(_, _)                   =>
          finish()
        case TransformResultFlipped(_, _)            =>
          finish()
        case TransformLast2Results(_)                =>
          finish()
        case TransformResultToOption(_)              =>
          finish()
        case PopResultPushOp(_)                      =>
          finish()
        case SkipOnFailure2                          =>
          finish()
        case SkipOnSuccess2(_, _)                    =>
          finish()
        case PushChunkBuilder(sizeHint)              =>
          collect(opStack.head, opStack.tail, posCount, names, sizeHint :: builderHints)
        case ProcessRepeatedElement(_, _, _)         =>
          finish()
        case Cut                                     =>
          finish()
        case BacktrackOnFailure                      =>
          finish()
        case PushCurrentPosition()                   =>
          finish()
        case CheckEnd()                              =>
          finish()
      }
    }

    collect(op, Nil, 0, Nil, Nil)
  }

  case class InitialParser(
      op: ParserOp,
      initialStack: Stack[ParserOp],
      initialPositions: Array[Int],
      initialPositionIndex: Int,
      initialNames: List[String],
      initialBuilders: Chunk[Int]
  )
}
