package zio.parser.internal.stacksafe

import zio.parser.Parser.ParserError
import zio.parser.Regex
import zio.parser.internal.Stack
import zio.parser.internal.stacksafe.ParserOp.InitialParser
import zio.{Chunk, ChunkBuilder}

import scala.annotation.nowarn

/** Stack safe interpreter for Parser
  */
final class CharParserImpl[Err, Result](parser: InitialParser, source: String) {
  import ParserOp._

  def run(): Either[ParserError[Err], Result] = {

    val len = source.length

    // Operation stack; the next operation is returned to the main loop as a return value, further operations are
    // stacked here
    val opStack: Stack[ParserOp] = parser.initialStack.clone()
    var op: ParserOp             = parser.op

    // Result stacks and explicit variables for the top two results. If success is null, a failure value must be
    // pushed. If success is not null, failure is not pushed The lastSuccess and lastFailure variables are either both
    // null (when no result yet) or one of them have value.
    var lastSuccess1: AnyRef                       = null
    var lastFailure1: ParserError[Any]             = null
    var lastSuccess2: AnyRef                       = null
    var lastFailure2: ParserError[Any]             = null
    val successResultStack: Stack[AnyRef]          = Stack()
    val failedResultStack: Stack[ParserError[Any]] = Stack()

    var lastIgnoredError: ParserError[Any] = null

    // Name stack for tracing
    var nameStack: List[String] = parser.initialNames

    // Stack of stored positions for branch verification / backtrack
    val storedPositions: Array[Int] = parser.initialPositions.clone()
    var storedPositionIndex: Int    = parser.initialPositionIndex

    // Stack and top value of chunk builders used by the Repeat operation
    val builderStack: Stack[ChunkBuilder[Any]] = Stack()
    var lastBuilder: ChunkBuilder[Any]         = null

    val ibs = parser.initialBuilders
    if (ibs.length > 0) {
      lastBuilder = ChunkBuilder.make(ibs(0))

      var idx = 1
      while (idx < ibs.length) {
        builderStack.push(ChunkBuilder.make(ibs(idx)))
        idx = idx + 1
      }
    }

    // Position in the source stream
    var position: Int = 0

    while (op != null) {
//      println(s"[$position] [$lastSuccess1/$lastFailure1] [$lastSuccess2/$lastFailure2]  ${op.getClass.getSimpleName}")

      if (op.needsEmptyResultSlot) {
        if (lastSuccess2 != null) {
          successResultStack.push(lastSuccess2)
        } else if (lastFailure2 != null) {
          successResultStack.push(null)
          failedResultStack.push(lastFailure2)
        }
        lastSuccess2 = lastSuccess1
        lastFailure2 = lastFailure1
        lastSuccess1 = null
        lastFailure1 = null
      }
      op = op match {
        case PushOp2(a, b, pushBranchPosition)       =>
          if (pushBranchPosition) {
            storedPositions(storedPositionIndex) = position
            storedPositionIndex = storedPositionIndex + 1
          }
          opStack.push(a)
          b
        case PushOp3(a, b, c)                        =>
          opStack.push(a)
          opStack.push(b)
          c
        case PushOp4(a, b, c, d, pushBranchPosition) =>
          if (pushBranchPosition) {
            storedPositions(storedPositionIndex) = position
            storedPositionIndex = storedPositionIndex + 1
          }
          opStack.push(a)
          opStack.push(b)
          opStack.push(c)
          d
        case l @ Lazy(_)                             =>
          l.memoized
        case PushResult(success, failure, popFirst)  =>
          if (popFirst) {
            if (lastSuccess1 != null) {
              lastSuccess1 = success
              lastFailure1 = failure
            }
          } else {
            lastSuccess1 = success
            lastFailure1 = failure
          }
          opStack.pop()
        case PushCapturedResult()                    =>
          if (lastSuccess1 != null) {
            val stored = storedPositions(storedPositionIndex - 1)
            lastSuccess1 = source.slice(stored, position)
          }
          storedPositionIndex = storedPositionIndex - 1
          opStack.pop()
        case PushCurrentPosition()                   =>
          lastSuccess1 = position.asInstanceOf[AnyRef]
          opStack.pop()
        case CheckEnd()                              =>
          if (position < source.length) {
            lastSuccess1 = null
            lastFailure1 = ParserError.NotConsumedAll(nameStack, position)
          } else {
            lastSuccess1 = ().asInstanceOf[AnyRef]: @nowarn
            lastFailure1 = null
          }
          opStack.pop()
        case PushName(name)                          =>
          nameStack = name :: nameStack
          opStack.pop()
        case ParserOp.PopName                        =>
          nameStack = nameStack.tail
          opStack.pop()
        case ParserOp.ReadInputToResult              =>
          if (position < len) {
            position = position + 1
            lastSuccess1 = source(position - 1).asInstanceOf[AnyRef]
          } else {
            lastFailure1 = ParserError.UnexpectedEndOfInput
          }
          opStack.pop()

        case MatchSeq(sequence, as, createParserFailure) =>
          val pos0                      = position
          var pos                       = 0
          var failure: ParserError[Any] = null
          while (pos < sequence.length && failure == null) {
            if ((pos0 + pos) < len) {
              val item = source(pos0 + pos)
              if (item != sequence(pos)) {
                failure = ParserError.Failure(
                  nameStack,
                  pos0 + pos,
                  createParserFailure(pos, item)
                )
              }
            } else {
              failure = ParserError.UnexpectedEndOfInput
            }
            pos = pos + 1
          }

          if (failure != null) {
            position = pos0
            lastFailure1 = failure
          } else {
            position = position + pos
            lastSuccess1 = as
          }
          opStack.pop()

        case MatchRegex(regex, pushAs, failAs) =>
          val result = regex.test(position, source)
          if (result == Regex.NeedMoreInput) {
            lastFailure1 = ParserError.UnexpectedEndOfInput
          } else if (result == Regex.NotMatched) {
            failAs match {
              case Some(failure) =>
                lastFailure1 = ParserError.Failure(nameStack, position, failure)
              case None          =>
                lastSuccess1 = Chunk.empty
            }
          } else {
            val oldPosition = position
            position = result
            pushAs match {
              case RegexResultPush.MatchedChunk =>
                lastSuccess1 = Chunk.fromArray(source.slice(oldPosition, result).toCharArray)
              case RegexResultPush.SingleChar   =>
                lastSuccess1 = source(result - 1).asInstanceOf[AnyRef]
              case RegexResultPush.Ignored      =>
                lastSuccess1 = ().asInstanceOf[AnyRef]: @nowarn
            }
          }
          opStack.pop()

        case TransformResultEither(f) =>
          if (lastSuccess1 != null) {
            f(lastSuccess1) match {
              case Left(value)  =>
                lastSuccess1 = null
                lastFailure1 = ParserError.Failure(nameStack, position, value)
              case Right(value) =>
                lastSuccess1 = value.asInstanceOf[AnyRef]
            }
          }
          opStack.pop()

        case TransformResult(onSuccess, onFailure) =>
          if (lastSuccess1 != null && onSuccess != null) {
            lastSuccess1 = onSuccess(lastSuccess1).asInstanceOf[AnyRef]
          } else if (onFailure != null) {
            lastFailure1 = onFailure(lastFailure1)
          }
          opStack.pop()

        case TransformResultFlipped(onSuccess, onFailure) =>
          if (lastSuccess1 != null) {
            lastFailure1 = onSuccess(position, lastSuccess1)
            lastSuccess1 = null
          } else {
            lastSuccess1 = onFailure(position, lastFailure1).asInstanceOf[AnyRef]
            lastFailure1 = null
          }
          opStack.pop()

        case TransformLast2Results(strategy) =>
          if (
            lastSuccess1 != null && (lastSuccess2 != null ||
              strategy == PairTransformation.IgnoreFirstWrapSecondAsRight ||
              strategy == PairTransformation.IgnoreFirstKeepSecond)
          ) {
            strategy match {
              case PairTransformation.Zip(zip)                     =>
                lastSuccess1 = zip(lastSuccess2, lastSuccess1).asInstanceOf[AnyRef]
              case PairTransformation.KeepFirst                    =>
                lastSuccess1 = lastSuccess2
              case PairTransformation.KeepSecond                   =>
              case PairTransformation.IgnoreFirstKeepSecond        =>
              case PairTransformation.IgnoreFirstWrapSecondAsRight =>
                lastSuccess1 = Right(lastSuccess1)
            }
          } else {
            if (lastFailure2 != null) {
              if (lastFailure1 != null) {
                lastFailure1 = lastFailure2.addFailedBranch(lastFailure1)
              } else {
                lastFailure1 = lastFailure2
              }
            }
            lastSuccess1 = null
          }
          lastSuccess2 = successResultStack.pop()
          if (lastSuccess2 == null)
            lastFailure2 = failedResultStack.pop()
          else
            lastFailure2 = null
          opStack.pop()

        case TransformResultToOption(checkBranchPosition) =>
          val transform = !checkBranchPosition || {
            val storedPosition = storedPositions(storedPositionIndex - 1)
            storedPositionIndex = storedPositionIndex - 1
            position == storedPosition
          }
          if (transform || lastSuccess1 != null) {
            if (lastSuccess1 != null)
              lastSuccess1 = Some(lastSuccess1)
            else {
              lastSuccess1 = None
              lastFailure1 = null
            }
          }
          opStack.pop()

        case ParserOp.PopResultPushOp(f) =>
          if (lastSuccess1 != null) {
            val parserOp = f(lastSuccess1)

            // pop result stack ..
            lastSuccess1 = lastSuccess2
            lastFailure1 = lastFailure2
            lastSuccess2 = successResultStack.pop()
            if (lastSuccess2 == null)
              lastFailure2 = failedResultStack.pop()
            else
              lastFailure2 = null
            // ..pop result stack

            parserOp
          } else {
            opStack.pop()
          }

        case ParserOp.SkipOnFailure2 =>
          if (lastSuccess1 == null) {
            opStack.pop()
            opStack.pop()
          }
          opStack.pop()

        case SkipOnSuccess2(checkBranchPosition, transform) =>
          if (lastSuccess1 != null) {
            opStack.pop()
            opStack.pop()

            if (checkBranchPosition) {
              storedPositionIndex = storedPositionIndex - 1
            }

            if (transform != null)
              lastSuccess1 = transform(lastSuccess1).asInstanceOf[AnyRef]
          } else {
            val proceed = !checkBranchPosition || {
              val storedPosition = storedPositions(storedPositionIndex - 1)
              storedPositionIndex = storedPositionIndex - 1
              position == storedPosition
            }
            if (!proceed) {
              opStack.pop()
              opStack.pop()
            }
          }
          opStack.pop()

        case PushChunkBuilder(sizeHint) =>
          if (lastBuilder != null)
            builderStack.push(lastBuilder)
          lastBuilder = ChunkBuilder.make(sizeHint)
          opStack.pop()

        case ProcessRepeatedElement(parseElement, min, _) =>
          if (lastSuccess1 != null) {
            // parsed an item
            lastBuilder += lastSuccess1

            // pop result stack ..
            lastSuccess1 = lastSuccess2
            lastFailure1 = lastFailure2
            lastSuccess2 = successResultStack.pop()
            if (lastSuccess2 == null)
              lastFailure2 = failedResultStack.pop()
            else
              lastFailure2 = null
            // ..pop result stack

            opStack.push(op)
            parseElement
          } else {
            val builder = lastBuilder
            lastBuilder = builderStack.pop()

            val result = builder.result()
            if (result.length < min) {
              // not enough elements
              lastFailure1 = ParserError.UnexpectedEndOfInput
              opStack.pop()
            } else {
              lastIgnoredError = lastFailure1
              lastFailure1 = null
              lastSuccess1 = result
            }
            opStack.pop()
          }

        case ParserOp.Cut                =>
          if (lastSuccess1 != null) {
            var idx = 0
            while (idx < storedPositionIndex) {
              storedPositions(idx) = -1
              idx = idx + 1
            }
          }
          opStack.pop()
        case ParserOp.BacktrackOnFailure =>
          val stored = storedPositions(storedPositionIndex - 1)
          storedPositionIndex = storedPositionIndex - 1
          if (lastSuccess1 == null && stored >= 0) {
            position = stored
          }
          opStack.pop()
      }
    }

    if (lastSuccess1 == null) {
      Left(lastFailure1.asInstanceOf[ParserError[Err]])
    } else {
      Right(lastSuccess1.asInstanceOf[Result])
    }
  }
}

object CharParserImpl {
  val maxStoredPositions = 1024
}
