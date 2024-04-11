package zio.parser

import zio.test.Assertion._
import zio.test._

object StringParserErrorSpec extends ZIOSpecDefault {
  override def spec =
    suite("StringParserError")(
      failureTest(
        "should pretty print a Failure error",
        Syntax.char('y'),
        "x",
        """|x
           |^
           |error: Failure at position 0: not 'y'
           |""".stripMargin
      ),
      failureTest(
        "should pretty print an UnexpectedEndOfInput error",
        Syntax.char('y'),
        "",
        "Unexpected end of input"
      ),
      failureTest(
        "should pretty print a NotConsumedAll error",
        Syntax.char('y') <~ Syntax.end,
        "yyz",
        """|yyz
           | ^
           |error: Parser did not consume all input at position 1
           |""".stripMargin
      ),
      failureTest(
        "should pretty print an AllBranchesFailed error",
        Syntax.char('y').orElse(Syntax.char('z')),
        "x",
        "All branches failed: Failure at position 0: not 'y' and Failure at position 0: not 'z'"
      ),
      failureTest(
        "should pretty print an error with multiline input",
        Syntax.char('y'),
        "x\ny",
        """|x
           |^
           |error: Failure at position 0: not 'y'
           |y
           |""".stripMargin
      ),
      failureTest(
        "should point to the correct position in the input",
        Syntax.char('x').repeat <~ Syntax.end,
        "xxxyxx",
        """|xxxyxx
           |   ^
           |error: Parser did not consume all input at position 3
           |""".stripMargin
      ),
      failureTest(
        "should replace the following lines with an ellipsis",
        Syntax.char('y'),
        "x\n" + "y\n" * 100,
        """|x
           |^
           |error: Failure at position 0: not 'y'
           |y
           |y
           |...
           |""".stripMargin
      ),
      failureTest(
        "should replace the preceding and following lines with an ellipsis",
        (Syntax.digit.orElse(Syntax.whitespace)).repeat <~ Syntax.end,
        "1\n1\n1\n" + "y\n" + "1\n1\n1\n",
        """|...
           |1
           |1
           |y
           |^
           |error: Parser did not consume all input at position 6
           |1
           |1
           |...
           |""".stripMargin
      ),
      failureTest(
        "should print the failed parser name",
        (Syntax.digit.named("digit").orElse(Syntax.whitespace.named("whitespace"))).repeat <~
          Syntax.end.named("end"),
        "1 1 1 1 y 1 1 1",
        """|1 1 1 1 y 1 1 1
           |        ^
           |error: Parser did not consume all input at position 8 in end
           |""".stripMargin
      ),
      failureTest(
        "should print the parser name stack",
        ((Syntax.digit.named("digit") ~ Syntax.string("Keyword", ()).named("keyword"))
          .named("combined")),
        "5Keywor",
        "Unexpected end of input in keyword -> combined"
      )
    )

  private def failureTest[E, T](
      name: String,
      parser: Syntax[E, Char, Char, T],
      input: String,
      expectedErrorMessage: String
  ): Spec[Any, TestFailure[Nothing]] =
    test(name) {
      assert(parser.parseString(input).left.map(_.pretty))(isLeft(equalTo(expectedErrorMessage)))
    }
}
