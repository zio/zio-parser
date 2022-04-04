package zio.parser.examples

import zio.Chunk
import zio.parser.{Printer, _}
import zio.test.Assertion._
import zio.test._

object BashPrettyPrinterExample extends ZIOSpecDefault {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Bash pretty printer example")(
      suite("Conditions")(
        test("eq") {
          assert(
            printCondition.printString(
              BashConditions.StringEquals(
                BashConditions.Variable(BashVariables.Variable(BashIdentifier("TEST"))),
                BashConditions.Literal("something")
              )
            )
          )(
            isRight(equalTo("${TEST} == something"))
          ) &&
          assert(
            printCondition.printString(
              BashConditions.StringEquals(
                BashConditions.Variable(BashVariables.Variable(BashIdentifier("TEST"))),
                BashConditions.Literal("something longer")
              )
            )
          )(
            isRight(equalTo("${TEST} == \"something longer\""))
          )
        },
        test("not") {
          assert(
            printCondition.printString(
              BashConditions.Not(
                BashConditions.RegularFileExists(
                  BashConditions.Variable(BashVariables.Variable(BashIdentifier("FILENAME")))
                )
              )
            )
          )(
            isRight(equalTo("""! -f ${FILENAME}"""))
          )
        },
        test("TRUE literal") {
          assert(
            printCondition.printString(
              BashConditions.Literal("TRUE")
            )
          )(
            isRight(equalTo("""TRUE"""))
          )
        },
        test("empty string literal") {
          assert(
            printCondition.printString(
              BashConditions.Literal("")
            )
          )(
            isRight(equalTo(""""""""))
          )
        }
      ),
      suite("Variables")(
        test("variable") {
          assert(
            printVariable.printString(
              BashVariables.Variable(BashIdentifier("TEST_VARIABLE"))
            )
          )(
            isRight(equalTo("""TEST_VARIABLE"""))
          )
        },
        test("positional") {
          assert(
            printVariable.printString(
              BashVariables.Positional(4)
            )
          )(
            isRight(equalTo("""4"""))
          )
        }
      ),
      test("Identifier") {
        assert(
          printIdentifier.printString(
            BashIdentifier("TEST_VARIABLE")
          )
        )(
          isRight(equalTo("""TEST_VARIABLE"""))
        )
      },
      suite("Expressions")(
        test("literal") {
          assert(printExpression.printString(BashExpressions.Literal("something")))(
            isRight(equalTo("something"))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("something with whitespace")))(
            isRight(equalTo("\"something with whitespace\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("&")))(
            isRight(equalTo("\"&\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("+")))(
            isRight(equalTo("\"+\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("?")))(
            isRight(equalTo("\"?\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("[")))(
            isRight(equalTo("\"[\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("]")))(
            isRight(equalTo("\"]\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("`")))(
            isRight(equalTo("\"\\`\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("~")))(
            isRight(equalTo("\"~\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("\"")))(
            isRight(equalTo("\"\\\"\""))
          ) &&
          assert(printExpression.printString(BashExpressions.Literal("\\")))(
            isRight(equalTo("\"\\\\\""))
          )
        },
        test("read variable") {
          assert(
            printExpression
              .printString(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("XYZ"))))
          )(
            isRight(equalTo("${XYZ}"))
          ) &&
          assert(
            printExpression
              .printString(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("X"))))
          )(
            isRight(equalTo("$X"))
          )
        },
        test("read whole array") {
          assert(
            printExpression.printString(
              BashExpressions.ReadArray(BashVariables.Variable(BashIdentifier("LST")), BashArrayIndices.All)
            )
          )(
            isRight(equalTo("${LST[*]}"))
          )
        },
        test("read array element") {
          assert(
            printExpression.printString(
              BashExpressions.ReadArray(
                BashVariables.Variable(BashIdentifier("LST")),
                BashArrayIndices.Index(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("IDX"))))
              )
            )
          )(
            isRight(equalTo("${LST[${IDX}]}"))
          )
        },
        test("eval") {
          assert(
            printExpression.printString(
              BashExpressions.Eval(BashStatements.Command(BashExpressions.Literal("test"), Nil))
            )
          )(
            isRight(equalTo("$(test)"))
          ) &&
          assert(
            printExpression.printString(
              BashExpressions.Eval(
                BashStatements.Command(
                  BashExpressions.Literal("test"),
                  List(
                    BashExpressions.Literal("x"),
                    BashExpressions.Literal("something else"),
                    BashExpressions.Literal("y")
                  )
                )
              )
            )
          )(
            isRight(equalTo("$(test x \"something else\" y)"))
          )
        },
        test("conditional") {
          assert(
            printExpression.printString(
              BashExpressions.Conditional(
                BashConditions.StringEquals(
                  BashConditions.Variable(BashVariables.Variable(BashIdentifier("TEST"))),
                  BashConditions.Literal("something")
                )
              )
            )
          )(
            isRight(equalTo("[[ ${TEST} == something ]]"))
          )
        },
        test("interpolated") {
          assert(
            printExpression
              .printString(BashExpressions.Interpolated(List(BashExpressions.Literal("something"))))
          )(
            isRight(equalTo("\"something\""))
          ) &&
          assert(
            printExpression.printString(
              BashExpressions.Interpolated(List(BashExpressions.Literal("something with whitespace")))
            )
          )(
            isRight(equalTo("\"something with whitespace\""))
          ) &&
          assert(
            printExpression
              .printString(
                BashExpressions.Interpolated(
                  List(BashExpressions.Literal("something "), BashExpressions.Literal("with whitespace"))
                )
              )
          )(isRight(equalTo("\"something with whitespace\""))) &&
          assert(
            printExpression.printString(
              BashExpressions.Interpolated(
                List(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("TEST"))))
              )
            )
          )(isRight(equalTo("\"${TEST}\""))) &&
          assert(
            printExpression.printString(
              BashExpressions.Interpolated(
                List(
                  BashExpressions.Literal("some"),
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("TEST")))
                )
              )
            )
          )(isRight(equalTo("\"some${TEST}\""))) &&
          assert(
            printExpression.printString(
              BashExpressions.Interpolated(
                List(
                  BashExpressions.Literal("something which is a "),
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("TEST")))
                )
              )
            )
          )(isRight(equalTo("\"something which is a ${TEST}\"")))
        },
        test("eval arithmetic") {
          assert(
            printExpression.printString(
              BashExpressions.EvalArithmetic(
                BashArithmeticExpressions.Div(
                  BashArithmeticExpressions
                    .Add(
                      BashArithmeticExpressions.Variable(BashVariables.Variable(BashIdentifier("X"))),
                      BashArithmeticExpressions.Number(1)
                    ),
                  BashArithmeticExpressions.Number(2)
                )
              )
            )
          )(isRight(equalTo("$(( ($X + 1) / 2 ))")))
        }
      ),
      suite("Statements")(
        test("assignment") {
          assert(
            printStatement
              .printString(BashStatements.Assign(BashIdentifier("TEST"), BashExpressions.True))
          )(
            isRight(equalTo("TEST=true"))
          )
        },
        test("command") {
          assert(
            printStatement.printString(
              BashStatements.Command(BashExpressions.Literal("echo"), List(BashExpressions.Literal("hello world")))
            )
          )(
            isRight(equalTo("""echo "hello world""""))
          ) &&
          assert(
            printStatement.printString(
              BashStatements.Command(
                BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("AWS"))),
                List(BashExpressions.Literal("describe-instance"), BashExpressions.Literal("i-test"))
              )
            )
          )(
            isRight(equalTo("${AWS} describe-instance i-test"))
          ) &&
          assert(
            printStatement.printString(
              BashStatements.Command(
                BashExpressions.Literal("bc"),
                List(BashExpressions.Literal("-l")),
                Some(BashExpressions.Literal("5+5"))
              )
            )
          )(
            isRight(equalTo("bc -l <<< \"5+5\""))
          )
        },
        // TODO: reenable when there is built-in support for indentation
//        test("if-then-else") {
//          val statement =
//            BashStatements.IfThenElse(
//              conditional = BashExpressions.Conditional(
//                BashConditions.StringEquals(
//                  BashConditions.Variable(BashVariables.Variable(BashIdentifier("TEST"))),
//                  BashConditions.Literal("something")
//                )
//              ),
//              onTrue = BashStatements
//                .Command(BashExpressions.Literal("echo"), List(BashExpressions.Literal("TEST is something"))),
//              onFalse = BashStatements.Command(
//                BashExpressions.Literal("echo"),
//                List(
//                  BashExpressions.Interpolated(
//                    List(
//                      BashExpressions.Literal("TEST is not something but "),
//                      BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("TEST")))
//                    )
//                  )
//                )
//              )
//            )
//
//          assert(printStatement.printString(statement))(
//            isRight(
//              equalTo(
//                "if [[ ${TEST} == something ]]\nthen\n    echo \"TEST is something\"\nelse\n    echo \"TEST is not something but ${TEST}\"\nfi"
//              )
//            )
//          )
//        },
        test("declare") {
          assert(
            printStatement.printString(
              BashStatements
                .Declare(Set(BashDeclareOptions.Array, BashDeclareOptions.ReadOnly), BashIdentifier("LST"), None)
            )
          )(
            isRight(equalTo("declare -a -r LST"))
          ) &&
          assert(
            printStatement.printString(
              BashStatements.Declare(
                Set(BashDeclareOptions.ReadOnly),
                BashIdentifier("LST"),
                Some(BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("TMP"))))
              )
            )
          )(
            isRight(equalTo("declare -r LST=${TMP}"))
          )
        },
        test("local") {
          assert(
            printStatement
              .printString(
                BashStatements.Local(
                  Set.empty,
                  BashIdentifier("X"),
                  Some(BashExpressions.ReadVariable(BashVariables.Positional(5)))
                )
              )
          )(
            isRight(equalTo("local X=$5"))
          ) &&
          assert(
            printStatement
              .printString(
                BashStatements.Local(
                  Set(BashDeclareOptions.ReadOnly),
                  BashIdentifier("OTHER"),
                  Some(BashExpressions.Literal("test"))
                )
              )
          )(
            isRight(equalTo("local -r OTHER=test"))
          )
        },
        test("let") {
          assert(
            printStatement.printString(
              BashStatements.Let(
                List(
                  BashArithmeticExpressions.AssignRem(
                    BashVariables.Variable(BashIdentifier("TEST")),
                    BashArithmeticExpressions.Exponentiation(
                      BashArithmeticExpressions.Number(2),
                      BashArithmeticExpressions.Number(3)
                    )
                  )
                )
              )
            )
          )(isRight(equalTo("let \"TEST %= (2 ** 3)\"")))
        },
        test("eval") {
          assert(
            printStatement.printString(
              BashStatements.Eval(
                BashStatements.Assign(
                  BashIdentifier("testfn2__retvar"),
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("testfn2__tmp2")))
                )
              )
            )
          )(
            isRight(equalTo("eval testfn2__retvar=${testfn2__tmp2}"))
          )
        },
        test("array update") {
          assert(
            printStatement.printString(
              BashStatements.ArrayUpdate(
                BashIdentifier("TABLE"),
                BashExpressions.Literal("3"),
                BashExpressions.Literal("something")
              )
            )
          )(isRight(equalTo("TABLE[3]=something")))
        },
        test("sequence") {
          val statement =
            BashStatements.Sequence(
              List(
                BashStatements.Command(BashExpressions.Literal("echo"), List(BashExpressions.Literal("Hello world"))),
                BashStatements.Command(
                  BashExpressions.ReadVariable(BashVariables.Variable(BashIdentifier("AWS"))),
                  List(BashExpressions.Literal("describe-instance"), BashExpressions.Literal("i-test"))
                ),
                BashStatements.Command(
                  BashExpressions.Literal("bc"),
                  List(BashExpressions.Literal("-l")),
                  hereString = Some(BashExpressions.Literal("5+5"))
                )
              )
            )

          assert(printStatement.printString(statement))(isRight(equalTo("""echo "Hello world"
                              |${AWS} describe-instance i-test
                              |bc -l <<< "5+5"""".stripMargin)))
        }
      )
    )

  type PP[A] = Printer[Nothing, Char, A, Unit]

  def print(ch: Char): Printer[Nothing, Char, Any, Unit]   =
    Printer.print(ch)
  def printS(s: String): Printer[Nothing, Char, Any, Unit] =
    Printer.printString(s)

  val indentation                                = Printer.unit // TODO
  // Printer.anyString
//    .statefulTransform(
//      (value: String, _: Any) => Right((value)),
//      (_: Any, state: BashPrinterState) => Right((" " * (state.indentation * state.indentationSize), state))
//    )
  // .unit
  val newline: Printer[Nothing, Char, Any, Unit] = printS(System.lineSeparator()) ~> indentation

  val space: Printer[Nothing, Char, Any, Unit]  = print(' ')
  val dollar: Printer[Nothing, Char, Any, Unit] = print('$')

  def between(
      prefix: String,
      suffix: String,
      printer: PP[Any]
  ): Printer[Nothing, Char, Any, Unit] =
    printS(prefix) ~> printer ~> printS(suffix)

  def doubleQuoted(printer: PP[Any]): Printer[Nothing, Char, Any, Unit]    =
    between("\"", "\"", printer)
  def dollarQuoted(printer: PP[Any]): Printer[Nothing, Char, Any, Unit]    =
    between("$'", "'", printer)
  def curlyBracketed(printer: PP[Any]): Printer[Nothing, Char, Any, Unit]  =
    between("{", "}", printer)
  def squareBracketed(printer: PP[Any]): Printer[Nothing, Char, Any, Unit] =
    between("[", "]", printer)

  val printIdentifier: PP[BashIdentifier] = Printer.byValue((id: BashIdentifier) => printS(id.name))

  val printVariable: PP[BashVariable] = Printer.byValue((v: BashVariable) =>
    v match {
      case BashVariables.Variable(name)    => printIdentifier(name)
      case BashVariables.Positional(index) => printS(index.toString)
    }
  )

  def renderInterpolatedParts(parts: List[BashExpression]): List[(BashStringRequirements, PP[Any])] =
    parts.flatMap {
      case BashExpressions.Literal(lit)           =>
        prettyPrintBashString(lit) match {
          case (req, p) => List((req.needsQuotes, p)) // Require at least simple quotes in interpolated strings
        }
      case BashExpressions.ReadVariable(variable) =>
        List((BashStringRequirements.NeedQuotes, dollar ~> curlyBracketed(printVariable(variable))))
      case BashExpressions.Interpolated(subParts) =>
        renderInterpolatedParts(subParts)
      case other: BashExpression                  =>
        // TODO
        List((BashStringRequirements.NeedQuotes, printS(other.toString)))
    }

  private val charsNeedsQuoting: Set[Char] =
    Set('?', '+', '&', '[', ']', '~')

  private val charsNeedsEscape: Set[Char] =
    Set('\\', '"', '$', '`')

  sealed trait BashStringRequirements {
    def needsQuotes: BashStringRequirements
    def needsDollarQuotes: BashStringRequirements
  }

  object BashStringRequirements {

    case object NoRequirements extends BashStringRequirements {
      override val needsQuotes: BashStringRequirements       = NeedQuotes
      override val needsDollarQuotes: BashStringRequirements = NeedDollarQuotes
    }

    case object NeedQuotes extends BashStringRequirements {
      override val needsQuotes: BashStringRequirements       = NeedQuotes
      override val needsDollarQuotes: BashStringRequirements = NeedDollarQuotes
    }

    case object NeedDollarQuotes extends BashStringRequirements {
      override val needsQuotes: BashStringRequirements       = NeedDollarQuotes
      override val needsDollarQuotes: BashStringRequirements = NeedDollarQuotes
    }

  }

  private def normalizePartsWithQuoteRequirements(
      parts: List[(BashStringRequirements, PP[Any])]
  ): List[(BashStringRequirements, PP[Any])] =
    parts match {
      case Nil                    => Nil
      case (req, printer) :: rest =>
        val (sameReq, tail)             = rest.span { case (r, _) => r == req }
        val partsToMerge: List[PP[Any]] = printer :: sameReq.map { case (_, p) => p }
        val mergedParts                 = partsToMerge.foldLeft[PP[Any]](Printer.unit)(_ ~> _)
        (req, mergedParts) :: normalizePartsWithQuoteRequirements(tail)
    }

  private def prettyPrintBashString(string: String): (BashStringRequirements, PP[Any]) =
    if (string.isEmpty) {
      (BashStringRequirements.NeedQuotes, Printer.unit)
    } else {
      string.foldLeft[(BashStringRequirements, PP[Any])]((BashStringRequirements.NoRequirements, Printer.unit)) {
        case ((reqs, p), ch) if ch.isWhitespace                => (reqs.needsQuotes, p ~> print(ch))
        case ((reqs, p), ch) if charsNeedsQuoting.contains(ch) => (reqs.needsQuotes, p ~> print(ch))
        case ((reqs, p), ch) if charsNeedsEscape.contains(ch)  => (reqs.needsQuotes, p ~> print('\\') ~> print(ch))
        case ((reqs, p), ch) if ch.isControl                   =>
          (reqs.needsDollarQuotes, p ~> printS("\\0") ~> printS(ch.toInt.toOctalString))
        case ((reqs, p), ch)                                   => (reqs, p ~> print(ch))
      }
    }

  private def renderStringPart(
      requirements: BashStringRequirements,
      printer: PP[Any]
  ): PP[Any] =
    requirements match {
      case BashStringRequirements.NoRequirements   => printer
      case BashStringRequirements.NeedQuotes       => doubleQuoted(printer)
      case BashStringRequirements.NeedDollarQuotes => dollarQuoted(printer)
    }

  lazy val printExpression: PP[BashExpression] = Printer.byValue {
    case BashExpressions.Literal(lit) =>
      val inString                   = false // TODO
      val (requirements, litPrinter) = prettyPrintBashString(lit)

      (requirements, inString) match {
        case (BashStringRequirements.NoRequirements, false) => litPrinter
        case (BashStringRequirements.NoRequirements, true)  => doubleQuoted(litPrinter)
        case (BashStringRequirements.NeedQuotes, _)         => doubleQuoted(litPrinter)
        case (BashStringRequirements.NeedDollarQuotes, _)   => dollarQuoted(litPrinter)
      }

    case BashExpressions.ReadVariable(variable) =>
      variable match {
        case BashVariables.Variable(name) if name.name.length > 1 =>
          dollar ~> curlyBracketed(printVariable(variable).unit)
        case _                                                    => dollar ~> printVariable(variable).unit
      }

    case BashExpressions.ReadArray(variable, index) =>
      dollar ~> curlyBracketed(printVariable(variable).unit ~> squareBracketed(printArrayIndex(index)))
    case BashExpressions.Eval(statement)            => between("$(", ")", printStatement(statement))
    case BashExpressions.Conditional(condition)     => between("[[ ", " ]]", printCondition(condition))
    case BashExpressions.Interpolated(parts)        =>
      val renderedParts   = renderInterpolatedParts(parts)
      val normalizedParts = normalizePartsWithQuoteRequirements(renderedParts)
      normalizedParts.foldLeft[PP[Any]](Printer.unit) { case (s, (req, p)) =>
        s ~> renderStringPart(req, p)
      }

    case BashExpressions.EvalArithmetic(expression) => between("$(( ", " ))", printArithmenticExpression(expression))
    case BashExpressions.True                       => printS("true")
    case BashExpressions.False                      => printS("false")
    case BashExpressions.And(a, b)                  => printExpression(a) ~> space ~> printS("&&") ~> space ~> printExpression(b)
    case BashExpressions.Or(a, b)                   => printExpression(a) ~> space ~> printS("||") ~> space ~> printExpression(b)
  }

  def indented(inner: PP[Any]): PP[Any] =
    inner // TODO
//      .contramapState { (state: BashPrinterState) => state.copy(indentation = state.indentation + 1) }
//      .mapState { (state: BashPrinterState) => state.copy(indentation = state.indentation - 1) }

  lazy val printStatement: PP[BashStatement] = Printer.byValue {
    case BashStatements.Nop                                      => Printer.unit
    case BashStatements.Assign(target, expression)               =>
      printIdentifier(target) ~> print('=') ~> printExpression(expression)
    case BashStatements.Command(name, params, None)              =>
      printExpression.repeatWithSep(print(' ')).apply(Chunk.fromIterable(name :: params)).unit
    case BashStatements.Command(name, params, Some(hereString))  =>
      printExpression
        .repeatWithSep(print(' '))
        .apply(Chunk.fromIterable(name :: params ::: List(BashExpressions.Literal("<<<"), hereString)))
        .unit
    case BashStatements.IfThenElse(conditional, onTrue, onFalse) =>
      printS("if") ~> space ~> printExpression(conditional) ~> newline ~>
        printS("then") ~>
        indented(newline ~> printStatement(onTrue)) ~> newline ~>
        printS("else") ~>
        indented(newline ~> printStatement(onFalse)) ~> newline ~>
        printS("fi")
    case BashStatements.Declare(options, name, initialValue)     =>
      val opts   = if (options.isEmpty) {
        Printer.unit
      } else {
        space ~> printDeclareOption.repeatWithSep(print(' ')).apply(Chunk.fromIterable(options))
      }
      val prefix = printS("declare") ~> opts ~> space ~> printS(name.name)
      initialValue match {
        case Some(value) => prefix ~> print('=') ~> printExpression(value)
        case None        => prefix
      }
    case BashStatements.Local(options, name, initialValue)       =>
      val opts: PP[Any] = if (options.isEmpty) {
        Printer.unit
      } else {
        space ~> printDeclareOption.repeatWithSep(print(' ')).apply(Chunk.fromIterable(options)).unit
      }
      val prefix        = printS("local") ~> opts ~> space ~> printS(name.name)
      initialValue match {
        case Some(value) => prefix ~> print('=') ~> printExpression(value)
        case None        => prefix
      }
    case BashStatements.Let(expression)                          =>
      val quotedExpr = Printer.byValue { (e: BashArithmeticExpression) =>
        print('"') ~> printArithmenticExpression(e) <~ print('"')
      }
      val printExprs = quotedExpr.repeatWithSep(print(' ')).unit
      printS("let") ~> space ~> printExprs(Chunk.fromIterable(expression))
    case BashStatements.Function(name, body)                     =>
      printS("function") ~> space ~> printIdentifier(name) ~> space ~> print('{') ~>
        indented(newline ~> printStatement(body)) ~> newline ~>
        print('}')
    case BashStatements.Eval(statement)                          =>
      printS("eval") ~> space ~> printStatement(statement)
    case BashStatements.ArrayUpdate(target, index, value)        =>
      printIdentifier(target) ~> squareBracketed(printExpression(index)) ~> print('=') ~> printExpression(value)
    case BashStatements.While(conditional, body)                 =>
      printS("while") ~> space ~> printExpression(conditional) ~> newline ~>
        printS("do") ~>
        indented(newline ~> printStatement(body)) ~> newline ~>
        printS("done")
    case BashStatements.Sequence(statements)                     =>
      printStatement.repeatWithSep(newline).apply(Chunk.fromIterable(statements)).unit
  }

  def needParentheses(expression: BashArithmeticExpression): Boolean =
    expression match {
      case BashArithmeticExpressions.Number(_)   => false
      case BashArithmeticExpressions.Variable(_) => false
      case _                                     => true
    }

  def parenthesed(expression: BashArithmeticExpression): PP[Any] =
    if (needParentheses(expression))
      between("(", ")", printArithmenticExpression(expression))
    else
      printArithmenticExpression(expression)

  def binary(
      x: BashArithmeticExpression,
      y: BashArithmeticExpression,
      op: String
  ): Printer[Nothing, Char, Any, Unit] =
    parenthesed(x) ~> space ~> printS(op) ~> space ~> parenthesed(y)

  def assignment(
      x: BashVariable,
      y: BashArithmeticExpression,
      op: String
  ): Printer[Nothing, Char, Any, Unit] =
    printVariable(x) ~> space ~> printS(op) ~> space ~> parenthesed(y)

  lazy val printArithmenticExpression: PP[BashArithmeticExpression] = Printer.byValue {
    case BashArithmeticExpressions.Number(value)                               => printS(value.toString)
    case BashArithmeticExpressions.Variable(variable)                          => printExpression(BashExpressions.ReadVariable(variable))
    case BashArithmeticExpressions.PostIncrement(x)                            => parenthesed(x) ~> printS("++")
    case BashArithmeticExpressions.PostDecrement(x)                            => parenthesed(x) ~> printS("--")
    case BashArithmeticExpressions.PreIncrement(x)                             => printS("++") ~> parenthesed(x)
    case BashArithmeticExpressions.PreDecrement(x)                             => printS("--") ~> parenthesed(x)
    case BashArithmeticExpressions.Minus(x)                                    => printS("-") ~> parenthesed(x)
    case BashArithmeticExpressions.Plus(x)                                     => printS("+") ~> parenthesed(x)
    case BashArithmeticExpressions.LogicalNot(x)                               => printS("!") ~> parenthesed(x)
    case BashArithmeticExpressions.BitwiseNot(x)                               => printS("~") ~> parenthesed(x)
    case BashArithmeticExpressions.Exponentiation(x, y)                        => binary(x, y, "**")
    case BashArithmeticExpressions.Add(x, y)                                   => binary(x, y, "+")
    case BashArithmeticExpressions.Sub(x, y)                                   => binary(x, y, "-")
    case BashArithmeticExpressions.Mul(x, y)                                   => binary(x, y, "*")
    case BashArithmeticExpressions.Div(x, y)                                   => binary(x, y, "/")
    case BashArithmeticExpressions.Rem(x, y)                                   => binary(x, y, "%")
    case BashArithmeticExpressions.BitwiseLeftShift(x, y)                      => binary(x, y, "<<")
    case BashArithmeticExpressions.BitwiseRightShift(x, y)                     => binary(x, y, ">>")
    case BashArithmeticExpressions.LessEq(x, y)                                => binary(x, y, "<=")
    case BashArithmeticExpressions.Less(x, y)                                  => binary(x, y, "<")
    case BashArithmeticExpressions.Greater(x, y)                               => binary(x, y, ">")
    case BashArithmeticExpressions.GreaterEq(x, y)                             => binary(x, y, ">=")
    case BashArithmeticExpressions.Equal(x, y)                                 => binary(x, y, "==")
    case BashArithmeticExpressions.NotEqual(x, y)                              => binary(x, y, "!=")
    case BashArithmeticExpressions.BitwiseAnd(x, y)                            => binary(x, y, "&")
    case BashArithmeticExpressions.BitwiseXor(x, y)                            => binary(x, y, "^")
    case BashArithmeticExpressions.BitwiseOr(x, y)                             => binary(x, y, "|")
    case BashArithmeticExpressions.LogicalAnd(x, y)                            => binary(x, y, "&&")
    case BashArithmeticExpressions.LogicalOr(x, y)                             => binary(x, y, "||")
    case BashArithmeticExpressions.Conditional(condition, trueCase, falseCase) =>
      parenthesed(condition) ~> space ~> printS("?") ~> space ~> parenthesed(trueCase) ~> space ~>
        printS(":") ~> space ~> parenthesed(falseCase)
    case BashArithmeticExpressions.Assign(x, y)                                => assignment(x, y, "=")
    case BashArithmeticExpressions.AssignMul(x, y)                             => assignment(x, y, "*=")
    case BashArithmeticExpressions.AssignDiv(x, y)                             => assignment(x, y, "/=")
    case BashArithmeticExpressions.AssignRem(x, y)                             => assignment(x, y, "%=")
    case BashArithmeticExpressions.AssignAdd(x, y)                             => assignment(x, y, "+=")
    case BashArithmeticExpressions.AssignSub(x, y)                             => assignment(x, y, "-=")
    case BashArithmeticExpressions.AssignShiftLeft(x, y)                       => assignment(x, y, "<<=")
    case BashArithmeticExpressions.AssignShiftRight(x, y)                      => assignment(x, y, ">>=")
    case BashArithmeticExpressions.AssignAnd(x, y)                             => assignment(x, y, "&=")
    case BashArithmeticExpressions.AssignOr(x, y)                              => assignment(x, y, "|=")
    case BashArithmeticExpressions.AssignXor(x, y)                             => assignment(x, y, "^=")
    case BashArithmeticExpressions.Comma(x, y)                                 => binary(x, y, ",")
  }

  def binaryCondition(
      x: BashCondition,
      y: BashCondition,
      op: String
  ): Printer[Nothing, Char, Any, Unit] =
    printCondition(x) ~> space ~> printS(op) ~> space ~> printCondition(y)

  def unaryCondition(
      x: BashCondition,
      op: String
  ): Printer[Nothing, Char, Any, Unit] =
    printS(op) ~> space ~> printCondition(x)

  lazy val printCondition: PP[BashCondition] = Printer.byValue {
    case BashConditions.Literal(value)                       => printExpression(BashExpressions.Literal(value))
    case BashConditions.Variable(variable)                   => printExpression(BashExpressions.ReadVariable(variable))
    case BashConditions.StringEquals(a, b)                   => binaryCondition(a, b, "==")
    case BashConditions.StringNotEquals(a, b)                => binaryCondition(a, b, "!=")
    case BashConditions.LexicographicLess(a, b)              => binaryCondition(a, b, "<")
    case BashConditions.LexicographicGreater(a, b)           => binaryCondition(a, b, ">")
    case BashConditions.Equals(a, b)                         => binaryCondition(a, b, "-eq")
    case BashConditions.NotEquals(a, b)                      => binaryCondition(a, b, "-ne")
    case BashConditions.Greater(a, b)                        => binaryCondition(a, b, "-gt")
    case BashConditions.GreaterEq(a, b)                      => binaryCondition(a, b, "-ge")
    case BashConditions.Less(a, b)                           => binaryCondition(a, b, "-lt")
    case BashConditions.LessEq(a, b)                         => binaryCondition(a, b, "-le")
    case BashConditions.Not(a)                               => unaryCondition(a, "!")
    case BashConditions.And(a, b)                            => binaryCondition(a, b, "&&")
    case BashConditions.Or(a, b)                             => binaryCondition(a, b, "||")
    case BashConditions.FileExists(a)                        => unaryCondition(a, "-a")
    case BashConditions.BlockFileExists(a)                   => unaryCondition(a, "-b")
    case BashConditions.CharacterFileExists(a)               => unaryCondition(a, "-c")
    case BashConditions.DirectoryExists(a)                   => unaryCondition(a, "-d")
    case BashConditions.RegularFileExists(a)                 => unaryCondition(a, "-f")
    case BashConditions.FileExistsWithSetGroupId(a)          => unaryCondition(a, "-g")
    case BashConditions.SymbolicLinkExists(a)                => unaryCondition(a, "-h")
    case BashConditions.FileExistsWithStickyBit(a)           => unaryCondition(a, "-k")
    case BashConditions.NamedPipeExists(a)                   => unaryCondition(a, "-p")
    case BashConditions.ReadableFileExists(a)                => unaryCondition(a, "-r")
    case BashConditions.NonEmptyFileExists(a)                => unaryCondition(a, "-s")
    case BashConditions.IsOpenTerminalFileDescriptor(a)      => unaryCondition(a, "-t")
    case BashConditions.FileExistsWithSetUserId(a)           => unaryCondition(a, "-u")
    case BashConditions.WriteableFileExists(a)               => unaryCondition(a, "-w")
    case BashConditions.ExecutableFileExists(a)              => unaryCondition(a, "-x")
    case BashConditions.FileExistsOwnedByEffectiveGroupId(a) => unaryCondition(a, "-G")
    case BashConditions.FileExistsModifiedSinceRead(a)       => unaryCondition(a, "-N")
    case BashConditions.SocketExists(a)                      => unaryCondition(a, "-S")
    case BashConditions.SameDeviceAndInode(a, b)             => binaryCondition(a, b, "-ef")
    case BashConditions.NewerThan(a, b)                      => binaryCondition(a, b, "-nt")
    case BashConditions.OlderThan(a, b)                      => binaryCondition(a, b, "-ot")
    case BashConditions.OptionEnabled(option)                => printS("-o") ~> space ~> printOption(option)
    case BashConditions.VariableSet(variable)                => printS("-v") ~> space ~> printVariable(variable)
    case BashConditions.NameReferenceSet(variable)           => printS("-R") ~> space ~> printVariable(variable)
    case BashConditions.ZeroLengthString(a)                  => unaryCondition(a, "-z")
    case BashConditions.NonZeroLengthString(a)               => unaryCondition(a, "-n")
  }

  val printOption: PP[BashOption] = Printer.byValue { opt =>
    printS(opt match {
      case BashOptions.AllExport   => "allexport"
      case BashOptions.BraceExpand => "braceexpand"
      case BashOptions.Emacs       => "emacs"
      case BashOptions.ErrExit     => "errexit"
      case BashOptions.ErrTrace    => "errtrace"
      case BashOptions.FuncTrace   => "functrace"
      case BashOptions.HashAll     => "hashall"
      case BashOptions.HistExpand  => "histexpand"
      case BashOptions.History     => "history"
      case BashOptions.IgnoreEof   => "ignoreeof"
      case BashOptions.Keyword     => "keyword"
      case BashOptions.Monitor     => "monitor"
      case BashOptions.NoClobber   => "noclobber"
      case BashOptions.NoExec      => "noexec"
      case BashOptions.NoGlob      => "noglob"
      case BashOptions.NoLog       => "nolog"
      case BashOptions.Notify      => "notify"
      case BashOptions.NoUnset     => "nounset"
      case BashOptions.OneCmd      => "onecmd"
      case BashOptions.Physical    => "physical"
      case BashOptions.PipeFail    => "pipefail"
      case BashOptions.Posix       => "posix"
      case BashOptions.Privileged  => "privileged"
      case BashOptions.Verbose     => "verbose"
      case BashOptions.Vi          => "vi"
      case BashOptions.Xtrace      => "xtrace"
    })
  }

  val printDeclareOption: PP[BashDeclareOption] = Printer.byValue {
    case BashDeclareOptions.Array    => printS("-a")
    case BashDeclareOptions.ReadOnly => printS("-r")
  }

  val printArrayIndex: PP[BashArrayIndex] = Printer.byValue {
    case BashArrayIndices.Index(index) => printExpression(index)
    case BashArrayIndices.All          => printS("*")
  }

  case class BashIdentifier(name: String)

  sealed trait BashStatement {
    def flatten: List[BashStatement] = List(this)
    def normalize: BashStatement     = this
  }
  object BashStatements      {
    case object Nop                                                                              extends BashStatement
    case class Assign(target: BashIdentifier, expression: BashExpression)                        extends BashStatement
    case class Command(name: BashExpression, params: List[BashExpression], hereString: Option[BashExpression] = None)
        extends BashStatement
    case class IfThenElse(conditional: BashExpression, onTrue: BashStatement, onFalse: BashStatement)
        extends BashStatement
    case class Declare(options: Set[BashDeclareOption], name: BashIdentifier, initialValue: Option[BashExpression])
        extends BashStatement
    case class Local(options: Set[BashDeclareOption], name: BashIdentifier, initialValue: Option[BashExpression])
        extends BashStatement
    case class Let(expression: List[BashArithmeticExpression])                                   extends BashStatement
    case class Function(name: BashIdentifier, body: BashStatement)                               extends BashStatement
    case class Eval(statement: BashStatement)                                                    extends BashStatement
    case class ArrayUpdate(target: BashIdentifier, index: BashExpression, value: BashExpression) extends BashStatement
    case class While(conditional: BashExpression, body: BashStatement)                           extends BashStatement
    case class Sequence(statements: List[BashStatement])                                         extends BashStatement {
      override def flatten: List[BashStatement] =
        statements.flatMap(_.flatten)

      override def normalize: BashStatement =
        flatten match {
          case Nil                     => Nop
          case List(statement)         => statement
          case ss: List[BashStatement] => Sequence(ss)
        }
    }
  }

  sealed trait BashExpression
  object BashExpressions {
    case class Literal(lit: String)                                     extends BashExpression
    case class ReadVariable(variable: BashVariable)                     extends BashExpression
    case class ReadArray(variable: BashVariable, index: BashArrayIndex) extends BashExpression
    case class Eval(statement: BashStatement)                           extends BashExpression
    case class Conditional(condition: BashCondition)                    extends BashExpression
    case class Interpolated(parts: List[BashExpression])                extends BashExpression
    case class EvalArithmetic(expression: BashArithmeticExpression)     extends BashExpression
    case object True                                                    extends BashExpression
    case object False                                                   extends BashExpression
    case class And(a: BashExpression, b: BashExpression)                extends BashExpression
    case class Or(a: BashExpression, b: BashExpression)                 extends BashExpression
  }

  sealed trait BashVariable
  object BashVariables {
    case class Variable(name: BashIdentifier) extends BashVariable
    case class Positional(index: Int)         extends BashVariable
  }

  sealed trait BashOption
  object BashOptions {
    case object AllExport   extends BashOption
    case object BraceExpand extends BashOption
    case object Emacs       extends BashOption
    case object ErrExit     extends BashOption
    case object ErrTrace    extends BashOption
    case object FuncTrace   extends BashOption
    case object HashAll     extends BashOption
    case object HistExpand  extends BashOption
    case object History     extends BashOption
    case object IgnoreEof   extends BashOption
    case object Keyword     extends BashOption
    case object Monitor     extends BashOption
    case object NoClobber   extends BashOption
    case object NoExec      extends BashOption
    case object NoGlob      extends BashOption
    case object NoLog       extends BashOption
    case object Notify      extends BashOption
    case object NoUnset     extends BashOption
    case object OneCmd      extends BashOption
    case object Physical    extends BashOption
    case object PipeFail    extends BashOption
    case object Posix       extends BashOption
    case object Privileged  extends BashOption
    case object Verbose     extends BashOption
    case object Vi          extends BashOption
    case object Xtrace      extends BashOption
  }

  sealed trait BashCondition
  object BashConditions {
    case class Literal(value: String)           extends BashCondition
    case class Variable(variable: BashVariable) extends BashCondition

    case class StringEquals(a: BashCondition, b: BashCondition)         extends BashCondition
    case class StringNotEquals(a: BashCondition, b: BashCondition)      extends BashCondition
    case class LexicographicLess(a: BashCondition, b: BashCondition)    extends BashCondition
    case class LexicographicGreater(a: BashCondition, b: BashCondition) extends BashCondition
    case class Equals(a: BashCondition, b: BashCondition)               extends BashCondition
    case class NotEquals(a: BashCondition, b: BashCondition)            extends BashCondition
    case class Greater(a: BashCondition, b: BashCondition)              extends BashCondition
    case class GreaterEq(a: BashCondition, b: BashCondition)            extends BashCondition
    case class Less(a: BashCondition, b: BashCondition)                 extends BashCondition
    case class LessEq(a: BashCondition, b: BashCondition)               extends BashCondition

    case class Not(a: BashCondition)                   extends BashCondition
    case class And(a: BashCondition, b: BashCondition) extends BashCondition
    case class Or(a: BashCondition, b: BashCondition)  extends BashCondition

    case class FileExists(a: BashCondition)                        extends BashCondition
    case class BlockFileExists(a: BashCondition)                   extends BashCondition
    case class CharacterFileExists(a: BashCondition)               extends BashCondition
    case class DirectoryExists(a: BashCondition)                   extends BashCondition
    case class RegularFileExists(a: BashCondition)                 extends BashCondition
    case class FileExistsWithSetGroupId(a: BashCondition)          extends BashCondition
    case class SymbolicLinkExists(a: BashCondition)                extends BashCondition
    case class FileExistsWithStickyBit(a: BashCondition)           extends BashCondition
    case class NamedPipeExists(a: BashCondition)                   extends BashCondition
    case class ReadableFileExists(a: BashCondition)                extends BashCondition
    case class NonEmptyFileExists(a: BashCondition)                extends BashCondition
    case class IsOpenTerminalFileDescriptor(a: BashCondition)      extends BashCondition
    case class FileExistsWithSetUserId(a: BashCondition)           extends BashCondition
    case class WriteableFileExists(a: BashCondition)               extends BashCondition
    case class ExecutableFileExists(a: BashCondition)              extends BashCondition
    case class FileExistsOwnedByEffectiveGroupId(a: BashCondition) extends BashCondition
    case class FileExistsModifiedSinceRead(a: BashCondition)       extends BashCondition
    case class SocketExists(a: BashCondition)                      extends BashCondition

    case class SameDeviceAndInode(a: BashCondition, b: BashCondition) extends BashCondition
    case class NewerThan(a: BashCondition, b: BashCondition)          extends BashCondition
    case class OlderThan(a: BashCondition, b: BashCondition)          extends BashCondition
    case class OptionEnabled(option: BashOption)                      extends BashCondition
    case class VariableSet(variable: BashVariable)                    extends BashCondition
    case class NameReferenceSet(variable: BashVariable)               extends BashCondition

    case class ZeroLengthString(a: BashCondition)    extends BashCondition
    case class NonZeroLengthString(a: BashCondition) extends BashCondition
  }

  sealed trait BashArrayIndex
  object BashArrayIndices {
    case class Index(index: BashExpression) extends BashArrayIndex
    case object All                         extends BashArrayIndex
  }

  sealed trait BashDeclareOption
  object BashDeclareOptions {
    case object Array    extends BashDeclareOption
    case object ReadOnly extends BashDeclareOption
  }

  sealed trait BashArithmeticExpression
  object BashArithmeticExpressions {
    case class Number(value: Int)                         extends BashArithmeticExpression // n
    case class Variable(variable: BashVariable)           extends BashArithmeticExpression // $X
    case class PostIncrement(x: BashArithmeticExpression) extends BashArithmeticExpression // x++
    case class PostDecrement(x: BashArithmeticExpression) extends BashArithmeticExpression // x--
    case class PreIncrement(x: BashArithmeticExpression)  extends BashArithmeticExpression // ++x
    case class PreDecrement(x: BashArithmeticExpression)  extends BashArithmeticExpression // --x
    case class Minus(x: BashArithmeticExpression)         extends BashArithmeticExpression // -x
    case class Plus(x: BashArithmeticExpression)          extends BashArithmeticExpression // +x
    case class LogicalNot(x: BashArithmeticExpression)    extends BashArithmeticExpression // !x
    case class BitwiseNot(x: BashArithmeticExpression)    extends BashArithmeticExpression // ~x
    case class Exponentiation(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x ** y
    case class Add(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x + y
    case class Sub(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x - y
    case class Mul(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x * y
    case class Div(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x / y
    case class Rem(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x % y
    case class BitwiseLeftShift(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x << y
    case class BitwiseRightShift(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x >> y
    case class LessEq(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x <= y
    case class Less(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x < y
    case class Greater(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x > y
    case class GreaterEq(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x >= y
    case class Equal(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x == y
    case class NotEqual(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x != y
    case class BitwiseAnd(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x & y
    case class BitwiseXor(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x ^ y
    case class BitwiseOr(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x | y
    case class LogicalAnd(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x && y
    case class LogicalOr(x: BashArithmeticExpression, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x || y
    case class Conditional(
        condition: BashArithmeticExpression,
        trueCase: BashArithmeticExpression,
        falseCase: BashArithmeticExpression
    ) extends BashArithmeticExpression // condition ? trueCase : falseCase
    case class Assign(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x = y
    case class AssignMul(x: BashVariable, y: BashArithmeticExpression)       extends BashArithmeticExpression // x *= y
    case class AssignDiv(x: BashVariable, y: BashArithmeticExpression)       extends BashArithmeticExpression // x /= y
    case class AssignRem(x: BashVariable, y: BashArithmeticExpression)       extends BashArithmeticExpression // x %= y
    case class AssignAdd(x: BashVariable, y: BashArithmeticExpression)       extends BashArithmeticExpression // x += y
    case class AssignSub(x: BashVariable, y: BashArithmeticExpression)       extends BashArithmeticExpression // x -= y
    case class AssignShiftLeft(x: BashVariable, y: BashArithmeticExpression) extends BashArithmeticExpression // x <<= y
    case class AssignShiftRight(x: BashVariable, y: BashArithmeticExpression)
        extends BashArithmeticExpression // x >>= y
    case class AssignAnd(x: BashVariable, y: BashArithmeticExpression)         extends BashArithmeticExpression // x &= y
    case class AssignOr(x: BashVariable, y: BashArithmeticExpression)          extends BashArithmeticExpression // x |= y
    case class AssignXor(x: BashVariable, y: BashArithmeticExpression)         extends BashArithmeticExpression // x ^= y
    case class Comma(x: BashArithmeticExpression, y: BashArithmeticExpression) extends BashArithmeticExpression // x, y
  }
}
