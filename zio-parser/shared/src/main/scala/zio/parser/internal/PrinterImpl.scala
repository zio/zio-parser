package zio.parser.internal

import zio.Chunk
import zio.parser.Parser.ParserError
import zio.parser.Printer
import zio.parser.target.Target

import scala.annotation.nowarn

/** Interpreter for Printer
  */
class PrinterImpl[Err, Out, Value](printer: Printer[Err, Out, Value]) {
  type ErasedPrinter = Printer[Any, Any, Any]
  case class Cont(f: Either[Any, Any] => (ErasedPrinter, Any, Option[Cont]))

  def run(value: Value, output: Target[Out]): Either[Err, Unit] = {
    var input: Any               = value
    var current: Any             = printer
    var result: Either[Any, Any] = Right(())
    val stack: Stack[Cont]       = Stack()

    def finish(r: Either[Any, Unit]): Unit =
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
      (current: @nowarn) match {
        case l @ Printer.Lazy(_) =>
          current = l.memoized

        case Printer.Succeed(_) =>
          finish(Right(()))

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
            case Right(_)      => (Printer.Succeed(()), oldInput, None)
          })

        case Printer.Passthrough() =>
          output.write(input.asInstanceOf[Out])
          finish(Right(()))

        case parseRegex @ Printer.ParseRegex(_, Some(failure)) =>
          val chunk = input.asInstanceOf[Chunk[Char]]
          if (parseRegex.compiledRegex.test(0, new String(chunk.toArray)) >= 0) {
            for (out <- input.asInstanceOf[Chunk[Out]])
              output.write(out)
            finish(Right(()))
          } else {
            finish(Left(failure))
          }
        case Printer.ParseRegex(_, None)                       =>
          for (out <- input.asInstanceOf[Chunk[Out]])
            output.write(out)
          finish(Right(()))
        case Printer.SkipRegex(_, as)                          =>
          for (out <- as.asInstanceOf[Chunk[Out]])
            output.write(out)
          finish(Right(()))

        case parseRegex @ Printer.ParseRegexLastChar(_, Some(failure)) =>
          val char = input.asInstanceOf[Char]
          if (parseRegex.compiledRegex.test(0, char.toString) > 0) {
            output.write(input.asInstanceOf[Out])
            finish(Right(()))
          } else {
            finish(Left(failure))
          }
        case Printer.ParseRegexLastChar(_, None)                       =>
          output.write(input.asInstanceOf[Out])
          finish(Right(()))

        case Printer.MapError(syntax, f) =>
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(f.asInstanceOf[Any => Any](failure)), input, None)
            case Right(_)      => (Printer.Succeed(()), input, None)
          })

        case Printer.Ignore(syntax, to, from) =>
          if (input == to) {
            val oldInput = input
            input = from
            current = syntax
            stack.push(Cont {
              case Left(failure) => (Printer.Fail(failure), oldInput, None)
              case Right(_)      => (Printer.Succeed(()), oldInput, None)
            })
          } else {
            finish(Left(Printer.Failed(ParserError.UnknownFailure(Nil, 0)))) // TODO
          }

        case Printer.Contramap(syntax, from) =>
          val oldInput = input
          input = from.asInstanceOf[Any => Any](input)
          current = syntax
          stack.push(Cont {
            case Left(failure) => (Printer.Fail(failure), oldInput, None)
            case Right(_)      => (Printer.Succeed(()), oldInput, None)
          })

        case Printer.ContramapEither(syntax, from) =>
          val oldInput = input
          from.asInstanceOf[Any => Either[Any, Any]](input) match {
            case Left(failure)   =>
              finish(Left(failure))
            case Right(newInput) =>
              input = newInput
              current = syntax
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(_)      =>
                  (Printer.Succeed(()), oldInput, None)
              })
          }

        case Printer.Zip(left, right, unzipValue) =>
          val oldInput         = input
          val (valueA, valueB) = unzipValue.asInstanceOf[Any => (Any, Any)](input)
          current = left
          input = valueA

          val k1 = Cont {
            case Left(failure) =>
              (Printer.Fail(failure), oldInput, None)
            case Right(_)      =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure) => (Printer.Fail(failure), oldInput, None)
                  case Right(_)      =>
                    (Printer.Succeed(()), oldInput, None)
                }
              )
              (right.asInstanceOf[ErasedPrinter], valueB, Some(k2))
          }
          stack.push(k1)
        case Printer.ZipLeft(left, right)         =>
          val oldInput = input
          val valueA   = input
          val valueB   = ()
          current = left
          input = valueA

          val k1 = Cont {
            case Left(failure) =>
              (Printer.Fail(failure), oldInput, None)
            case Right(_)      =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure) => (Printer.Fail(failure), oldInput, None)
                  case Right(_)      => (Printer.Succeed(()), oldInput, None)
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
            case Left(failure) =>
              (Printer.Fail(failure), oldInput, None)
            case Right(_)      =>
              val k2 = Cont((rightResult: Either[Any, Any]) =>
                rightResult match {
                  case Left(failure) => (Printer.Fail(failure), oldInput, None)
                  case Right(_)      => (Printer.Succeed(()), oldInput, None)
                }
              )
              (right.asInstanceOf[ErasedPrinter], valueB, Some(k2))
          }
          stack.push(k1)

        case Printer.OrElseEither(left, right) =>
          val oldInput    = input
          val eitherInput = input.asInstanceOf[Either[Any, Any]]
          eitherInput match {
            case Left(leftInput) =>
              input = leftInput
              current = left
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(_)      => (Printer.Succeed(()), oldInput, None)
              })

            case Right(rightInput) =>
              input = rightInput
              current = right
              stack.push(Cont {
                case Left(failure) => (Printer.Fail(failure), oldInput, None)
                case Right(_)      => (Printer.Succeed(()), oldInput, None)
              })

          }

        case Printer.OrElse(left, right) =>
          current = left
          val capture = output.capture()

          stack.push(Cont {
            case Left(_)  =>
              output.drop(capture)
              (right.asInstanceOf[ErasedPrinter], input, None)
            case Right(_) =>
              output.emit(capture)
              (Printer.Succeed(()), input, None)
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
                case Right(_)      => (Printer.Succeed(()), oldInput, None)
              })

            case None =>
              finish(Right(()))
          }

        case rep @ Printer.Repeat(inner, _, _) =>
          val inputChunk = input.asInstanceOf[Chunk[Any]]
          if (inputChunk.isEmpty) {
            finish(Right(()))
          } else {
            val head = inputChunk.head
            val tail = inputChunk.tail

            current = inner
            input = head
            stack.push(Cont {
              case Left(failure) =>
                (Printer.Fail(failure), inputChunk, None)
              case Right(_)      =>
                (
                  rep.asInstanceOf[ErasedPrinter],
                  tail,
                  Some(Cont {
                    case Left(failure) =>
                      (Printer.Fail(failure), inputChunk, None)
                    case Right(_)      =>
                      (Printer.Succeed(()), inputChunk, None)
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
