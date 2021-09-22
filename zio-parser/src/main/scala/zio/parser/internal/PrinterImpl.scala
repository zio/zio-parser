package zio.parser.internal

import zio.Chunk
import zio.parser.Parser.ParserError
import zio.parser.Printer
import zio.parser.target.Target

/** Interpreter for Printer
  */
class PrinterImpl[Err, Out, Value, Result](printer: Printer[Err, Out, Value, Result]) {
  type ErasedPrinter = Printer[Any, Any, Any, Any]
  case class Cont(f: Either[Any, Any] => (ErasedPrinter, Any, Option[Cont]))

  def run(value: Value, output: Target[Out]): Either[Err, Unit] = {
    var input: Any               = value
    var current: Any             = printer
    var result: Either[Any, Any] = Right(())
    val stack: Stack[Cont]       = Stack()

    def finish(r: Either[Any, Any]): Unit =
      if (stack.isEmpty) {
        result = r
        current = null
      } else {
        val k                    = stack.pop()
        val (next, nextI, nextK) = k.f(r)
        current = next
        input = nextI
        nextK.foreach(stack.push)
      }

    while (current != null) {
      current match {
        case l @ Printer.Lazy(_) =>
          current = l.memoized

        case Printer.Succeed(value) =>
          finish(Right(value))

        case Printer.Fail(failure) =>
          finish(Left(failure))

        case Printer.FlatMapValue(f) =>
          current = f.asInstanceOf[Any => ErasedPrinter](input)

        case Printer.ProvideValue(syntax, value) =>
          val oldInput = input
          input = value
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(failure), oldInput, None)
            case Right(value)  => (Printer.Succeed(value), oldInput, None)
          })

        case Printer.Passthrough() =>
          output.write(input.asInstanceOf[Out])
          finish(Right(input))

        case parseRegex @ Printer.ParseRegex(regex, Some(failure)) =>
          val chunk = input.asInstanceOf[Chunk[Char]]
          if (parseRegex.compiledRegex.test(0, new String(chunk.toArray)) >= 0) {
            for (out <- input.asInstanceOf[Chunk[Out]])
              output.write(out)
            finish(Right(input))
          } else {
            finish(Left(failure))
          }
        case Printer.ParseRegex(_, None)                           =>
          for (out <- input.asInstanceOf[Chunk[Out]])
            output.write(out)
          finish(Right(input))
        case Printer.SkipRegex(regex, as)                          =>
          for (out <- as.asInstanceOf[Chunk[Out]])
            output.write(out)
          finish(Right(()))

        case parseRegex @ Printer.ParseRegexLastChar(regex, Some(failure)) =>
          val char = input.asInstanceOf[Char]
          if (parseRegex.compiledRegex.test(0, char.toString) > 0) {
            output.write(input.asInstanceOf[Out])
            finish(Right(input))
          } else {
            finish(Left(failure))
          }
        case Printer.ParseRegexLastChar(_, None)                           =>
          output.write(input.asInstanceOf[Out])
          finish(Right(input))

        case Printer.MapError(syntax, f) =>
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(f.asInstanceOf[Any => Any](failure)), input, None)
            case Right(value)  => (Printer.Succeed(value), input, None)
          })

        case Printer.Ignore(syntax, to, from) =>
          if (input == to) {
            val oldInput = input
            input = from
            current = syntax
            stack.push(Cont {
              case Left(failure) => (Printer.Fail(failure), oldInput, None)
              case Right(value)  =>
                (Printer.Succeed(to), oldInput, None)
            })
          } else {
            finish(Left(Printer.Failed(ParserError.UnknownFailure(Nil, 0)))) // TODO
          }

        case Printer.Transform(syntax, to, from) =>
          val oldInput = input
          input = from.asInstanceOf[Any => Any](input)
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(failure), oldInput, None)
            case Right(value)  =>
              (Printer.Succeed(to.asInstanceOf[Any => Any](value)), oldInput, None)
          })

        case Printer.TransformEither(syntax, to, from) =>
          val oldInput = input
          from.asInstanceOf[Any => Either[Any, Any]](input) match {
            case Left(failure)   =>
              finish(Left(failure))
            case Right(newInput) =>
              input = newInput
              current = syntax
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(value)  =>
                  to.asInstanceOf[Any => Either[Any, Any]](value) match {
                    case Left(failure) =>
                      (Printer.Fail(failure), oldInput, None)
                    case Right(value)  =>
                      (Printer.Succeed(value), oldInput, None)
                  }
              })
          }

        case Printer.Zip(left, right)      =>
          val oldInput         = input
          val (valueA, valueB) = input.asInstanceOf[(Any, Any)]
          current = left
          input = valueA

          val k1 = Cont {
            case Left(failure)     =>
              (Printer.Fail(failure), oldInput, None)
            case Right(leftResult) =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure)      => (Printer.Fail(failure), oldInput, None)
                  case Right(rightResult) => (Printer.Succeed((leftResult, rightResult)), oldInput, None)
                }
              )
              (right.asInstanceOf[ErasedPrinter], valueB, Some(k2))
          }
          stack.push(k1)
        case Printer.ZipLeft(left, right)  =>
          val oldInput = input
          val valueA   = input
          val valueB   = ()
          current = left
          input = valueA

          val k1 = Cont {
            case Left(failure)     =>
              (Printer.Fail(failure), oldInput, None)
            case Right(leftResult) =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure)      => (Printer.Fail(failure), oldInput, None)
                  case Right(rightResult) => (Printer.Succeed(leftResult), oldInput, None)
                }
              )
              (right.asInstanceOf[ErasedPrinter], valueB, Some(k2))
          }
          stack.push(k1)
        case Printer.ZipRight(left, right) =>
          val oldInput = input
          val valueA   = ()
          val valueB   = input
          current = left
          input = valueA

          val k1 = Cont {
            case Left(failure)     =>
              (Printer.Fail(failure), oldInput, None)
            case Right(leftResult) =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure)      => (Printer.Fail(failure), oldInput, None)
                  case Right(rightResult) => (Printer.Succeed(rightResult), oldInput, None)
                }
              )
              (right.asInstanceOf[ErasedPrinter], valueB, Some(k2))
          }
          stack.push(k1)

        case Printer.FlatMapResult(syntax, f) =>
          val oldInput = input
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(failure), oldInput, None)
            case Right(result) => (f.asInstanceOf[Any => ErasedPrinter](result), oldInput, None)
          })

        case Printer.OrElseEither(left, right) =>
          val oldInput    = input
          val eitherInput = input.asInstanceOf[Either[Any, Any]]
          eitherInput match {
            case Left(leftInput) =>
              input = leftInput
              current = left
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(value)  => (Printer.Succeed(Left(value)), oldInput, None)
              })

            case Right(rightInput) =>
              input = rightInput
              current = right
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(value)  => (Printer.Succeed(Right(value)), oldInput, None)
              })

          }

        case Printer.OrElse(left, right) =>
          current = left
          val capture = output.capture()

          stack.push(Cont {
            case Left(_)       =>
              output.drop(capture)
              (right.asInstanceOf[ErasedPrinter], input, None)
            case Right(result) =>
              output.emit(capture)
              (Printer.Succeed(result), input, None)
          })

        case Printer.Optional(inner) =>
          val oldInput = input
          val optInput = input.asInstanceOf[Option[Any]]

          optInput match {
            case Some(someInput) =>
              input = someInput
              current = inner
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(value)  => (Printer.Succeed(Some(value)), oldInput, None)
              })

            case None =>
              finish(Right(None))
          }

        case rep @ Printer.Repeat(inner, _, _) =>
          val inputChunk = input.asInstanceOf[Chunk[Any]]
          if (inputChunk.isEmpty) {
            finish(Right(Chunk.empty))
          } else {
            val head = inputChunk.head
            val tail = inputChunk.tail

            current = inner
            input = head
            stack.push(Cont {
              case Left(failure)     =>
                (Printer.Fail(failure), inputChunk, None)
              case Right(headResult) =>
                (
                  rep.asInstanceOf[ErasedPrinter],
                  tail,
                  Some(Cont {
                    case Left(failure)     =>
                      (Printer.Fail(failure), inputChunk, None)
                    case Right(tailResult) =>
                      (Printer.Succeed(headResult +: tailResult.asInstanceOf[Chunk[Any]]), inputChunk, None)
                  })
                )
            })
          }
      }
    }

    result.left
      .map(_.asInstanceOf[Err])
      .map(_ => ())
  }
}
