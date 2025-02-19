package zio.parser.examples

import zio.parser.{Syntax, _}
import zio.test.Assertion._
import zio.test._

object ExpressionExample extends ZIOSpecDefault {

  sealed trait OpType
  case object Add extends OpType
  case object Sub extends OpType

  sealed trait Expr
  case class Const(value: Int)                      extends Expr
  case class Op(operator: OpType, a: Expr, b: Expr) extends Expr

  val const: Syntax[String, Char, Char, Expr]      = Syntax.anyChar
    .filter[String, Char](_.isDigit, "not a digit")
    .repeat
    .string
    .transformTo[String, Expr](
      s => Const(s.toInt),
      { case (n: Const) => n.value.toString },
      "Not a constant"
    ) ?? "constant"
  val operator: Syntax[String, Char, Char, OpType] =
    (Syntax.charIn('+').transformTo[String, OpType](_ => Add, { case Add => '+' }, "Not +") <>
      Syntax.charIn('-').transformTo[String, OpType](_ => Sub, { case Sub => '-' }, "Not -")) ?? "operator"
  val lparen: Syntax[String, Char, Char, Unit]     = Syntax.char('(')
  val rparen: Syntax[String, Char, Char, Unit]     = Syntax.char(')')

  lazy val subExpr: Syntax[String, Char, Char, Expr] =
    (expr ~ operator ~ expr)
      .transformTo[String, Expr](
        { case (a, op, b) =>
          Op(op, a, b)
        },
        { case (op: Op) => (op.a, op.operator, op.b) },
        "Not valid sub expression"
      ) ?? "sub expression"

  lazy val subExprInParens: Syntax[String, Char, Char, Expr] = (lparen ~> subExpr <~ rparen)

  lazy val expr: Syntax[String, Char, Char, Expr] =
    (subExprInParens | const) ?? "expression"

  val exampleExpr: Expr = Op(
    Sub,
    Op(Add, Op(Sub, Op(Add, Const(123), Const(456)), Const(789)), Op(Add, Const(0), Op(Add, Const(1), Const(2)))),
    Const(3)
  )

  override def spec: Spec[Environment, Any] =
    suite("Expression example")(
      test("Parses expression correctly") {
//        Debug.printParserTree(expr.asParser.optimized)
        val ast = expr.parseString("((((123+456)-789)+(0+(1+2)))-3)")
        assert(ast)(isRight(equalTo(exampleExpr)))
      },
      test("Prints expression correctly") {
        assert(expr.printString(exampleExpr))(isRight(equalTo("((((123+456)-789)+(0+(1+2)))-3)")))
      }
    )
}
