package zio.parser

import zio.Chunk
import zio.parser.Parser.ParserError
import zio.test.Assertion._
import zio.test._

object ParserSpec extends ZIOSpecDefault {
  private val charA: Syntax[String, Char, Char, Char, Char] = Syntax.char('a', "not a").as('a')
  private val charB: Syntax[String, Char, Char, Char, Char] = Syntax.char('b', "not b").as('b')

  case class TestCaseClass(a: String, b: Int)

  override def spec: ZSpec[Environment, Any] = {
    suite("Parsing")(
      List(ParserImplementation.StackSafe, ParserImplementation.Recursive).map { implementation =>
        def parserTest[E, T](name: String, syntax: Syntax[E, Char, Char, T, T], input: String)(
            assertion: Assertion[Either[ParserError[E], T]]
        ): ZSpec[Any, Nothing] = createParserTest(implementation)(name, syntax, input)(assertion)

        def parserTest_[E, T](name: String, parser: Parser[E, Char, T], input: String)(
            assertion: Assertion[Either[ParserError[E], T]]
        ): ZSpec[Any, Nothing] = createParserTest_(implementation)(name, parser, input)(assertion)

        suite(implementation.toString)(
          suite("Invertible syntax")(
            parserTest("succeed", Syntax.succeed("test"), "hello world")(isRight(equalTo("test"))),
            parserTest("end, passing", Syntax.anyChar.repeat0.string <~ Syntax.end, "hello")(
              isRight(equalTo("hello"))
            ),
            parserTest("end, failing", Syntax.digit.repeat0.string <~ Syntax.end, "123!!!")(
              isLeft(equalTo(ParserError.NotConsumedAll(None)))
            ),
            parserTest("char, passing", Syntax.char('x'), "x")(isRight(equalTo(()))),
            parserTest("char, failing", Syntax.char('y'), "x")(
              isLeft(equalTo(ParserError.Failure(Nil, 0, "not 'y'")))
            ),
            parserTest("charIn, passing #1", Syntax.charIn('A', 'B', 'C'), "AZXY")(isRight(equalTo('A'))),
            parserTest("charIn, passing #2", Syntax.charIn('A', 'B', 'C'), "BZXY")(isRight(equalTo('B'))),
            parserTest("charIn, passing #3", Syntax.charIn('A', 'B', 'C'), "CZXY")(isRight(equalTo('C'))),
            parserTest("charIn, passing #4", Syntax.charIn('A', 'B', 'C'), "ABCZXY")(isRight(equalTo('A'))),
            parserTest("charIn, failing", Syntax.charIn('A', 'B'), "ZABAB")(
              isLeft(equalTo(ParserError.Failure(Nil, 0, "Not the expected character (A, B)")))
            ),
            parserTest("anyChar", Syntax.anyChar, "h")(isRight(equalTo('h'))),
            parserTest("filtered char, passing", Syntax.anyChar.filter((ch: Char) => ch == 'h', "not h"), "h")(
              isRight(equalTo('h'))
            ),
            parserTest("filtered char, failing", Syntax.anyChar.filter((ch: Char) => ch == 'h', "not h"), "e")(
              isLeft(equalTo(ParserError.Failure(Nil, 1, "not h")))
            ),
            parserTest("transform", Syntax.anyChar.transform(_.toInt, (v: Int) => v.toChar), "h")(
              isRight(equalTo('h'.toInt))
            ),
            parserTest(
              "transformEither, failing",
              Syntax.anyChar.transformEither(_ => Left("bad"), (v: Int) => Right(v.toChar)),
              "hello"
            )(
              isLeft(equalTo(ParserError.Failure(Nil, 1, "bad")))
            ),
            parserTest("s ~ s", Syntax.anyChar ~ Syntax.anyChar, "he")(isRight(equalTo(('h', 'e')))),
            parserTest("s ~ s ~ s", Syntax.anyChar ~ Syntax.anyChar ~ Syntax.anyChar, "hel")(
              isRight(equalTo(('h', 'e', 'l')))
            ), {
              val a = Syntax.char('h').as(1)
              val b = Syntax.char('e') ~> Syntax.char('l').as(2)
              parserTest(
                "s ~ (s ~> s)",
                a ~ b,
                "hel"
              )(
                isRight(equalTo((1, 2)))
              )
            },
            parserTest(
              "s ~ s, failing left",
              Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a") ~ Syntax.anyChar,
              "he"
            )(
              isLeft(equalTo(ParserError.Failure(Nil, 1, "not a")))
            ),
            parserTest(
              "s ~ s, failing right",
              Syntax.anyChar ~ Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a"),
              "he"
            )(
              isLeft(equalTo(ParserError.Failure(Nil, 2, "not a")))
            ),
            parserTest(
              "anyChar.filter(isDigit)",
              Syntax.anyChar.filter((ch: Char) => ch.isDigit, "not digit").+.string,
              "123abc"
            )(
              isRight(equalTo("123"))
            ),
            parserTest("s <* s", Syntax.anyChar <~ Syntax.anyChar.asPrinted((), '?'), "he")(isRight(equalTo('h'))),
            parserTest("s *> s", Syntax.anyChar.asPrinted((), '?') ~> Syntax.anyChar, "he")(isRight(equalTo('e'))),
            parserTest(
              "s | s, left passing",
              charA | charB,
              "a"
            )(
              isRight(equalTo('a'))
            ),
            parserTest(
              "s | s, right passing",
              charA | charB,
              "b"
            )(
              isRight(equalTo('b'))
            ),
            parserTest(
              "captured s | s, right passing",
              Syntax.char('a', "not a").string | Syntax.char('b', "not b").string,
              "b"
            )(
              isRight(equalTo("b"))
            ),
            parserTest("s | s, failing", Syntax.char('a', "not a") | Syntax.char('b', "not b"), "c")(
              isLeft(
                equalTo(
                  ParserError
                    .AllBranchesFailed(ParserError.Failure(Nil, 0, "not a"), ParserError.Failure(Nil, 0, "not b"))
                )
              )
            ),
            parserTest(
              "s <+> s, left passing",
              charA <+> charB,
              "a"
            )(
              isRight(isLeft(equalTo('a')))
            ),
            parserTest(
              "s <+> s, right passing",
              charA <+> charB,
              "b"
            )(
              isRight(isRight(equalTo('b')))
            ),
            parserTest(
              "s <+> s, failing",
              charA <+> charB,
              "c"
            )(
              isLeft(
                equalTo(
                  ParserError.AllBranchesFailed(
                    ParserError.Failure(Nil, 0, "not a"),
                    ParserError.Failure(Nil, 0, "not b")
                  )
                )
              )
            ),
            parserTest(
              "s?, passing",
              (charA ~ charB).?,
              "ab"
            )(
              isRight(isSome(equalTo(('a', 'b'))))
            ),
            parserTest(
              "s?, not passing",
              (Syntax.char('a', "not a") ~ Syntax
                .char('b', "not b")).?,
              "aa"
            )(
              isRight(isNone)
            ),
            parserTest(
              "s?, terminating early",
              (Syntax.char('a', "not a") ~ Syntax
                .char('b', "not b")).?,
              "a"
            )(
              isRight(isNone)
            ),
            parserTest(
              "string, passing",
              Syntax.string("test", 1),
              "test"
            )(isRight(equalTo(1))),
            parserTest(
              "string, not passing",
              Syntax.string("test", 1),
              "tess"
            )(isLeft(equalTo(ParserError.Failure(Nil, 0, "Not 'test'")))),
            parserTest(
              "asString",
              (Syntax.char('a') <> Syntax.char('b')).repeatWithSep(Syntax.char(',')).string <~ Syntax
                .char('!'),
              "a,a,b,a,b,b!"
            )(isRight(equalTo("a,a,b,a,b,b"))),
            suite("repeat")(
              parserTest("repeat immediate mismatch", Syntax.char('a', "not a").repeat, "bc")(
                isLeft(equalTo(ParserError.Failure(Nil, 0, "not a")))
              ) @@ TestAspect.ignore, // With compiling to Regex it fails with UnexpectedEndOfInput - to be discussed
              parserTest("repeat immediate end of stream", Syntax.char('a', "not a").repeat, "")(
                isLeft(equalTo(ParserError.UnexpectedEndOfInput))
              ),
              parserTest("repeat 1", charA.repeat, "abc")(
                isRight(equalTo(Chunk('a')))
              ),
              parserTest("repeat 3", charA.repeat, "aaabc")(
                isRight(equalTo(Chunk('a', 'a', 'a')))
              ),
              parserTest("repeat until end", charA.repeat, "aaa")(
                isRight(equalTo(Chunk('a', 'a', 'a')))
              )
            ),
            suite("atLeast")(
              parserTest("atLeast 3, passing", Syntax.char('a').as('a').atLeast(3), "aaabc")(
                isRight(equalTo(Chunk('a', 'a', 'a')))
              ),
              parserTest("atLeast 3, failing", Syntax.char('a').as('a').atLeast(3), "aabca")(
                isLeft(equalTo(ParserError.UnexpectedEndOfInput))
              )
            ),
            suite("repeat0")(
              parserTest(
                "repeat0 immediate mismatch",
                Syntax.char('a', "not a").repeat0,
                "bc"
              )(
                isRight(equalTo(Chunk.empty))
              ),
              parserTest("repeat0 immediate end of stream", charA.repeat0, "")(
                isRight(equalTo(Chunk.empty))
              ),
              parserTest("repeat0 1", charA.repeat0, "abc")(
                isRight(equalTo(Chunk('a')))
              ),
              parserTest(
                "repeat0 3",
                charA.repeat0,
                "aaabc"
              )(
                isRight(equalTo(Chunk('a', 'a', 'a')))
              ),
              parserTest("repeat0 until end", charA.repeat0, "aaa")(
                isRight(equalTo(Chunk('a', 'a', 'a')))
              )
            ),
            suite("repeatWithSep")(
              parserTest("repeatWithSep 3", Syntax.anyChar.repeatWithSep(Syntax.char('-')), "a-b-c")(
                isRight(equalTo(Chunk('a', 'b', 'c')))
              )
            ),
            suite("repeatUntil")(
              parserTest(
                "repeatUntil 1",
                Syntax.anyChar.repeatUntil(Syntax.char('x') | Syntax.char('y')),
                "abcdy"
              )(isRight(equalTo(Chunk('a', 'b', 'c', 'd')))),
              parserTest(
                "repeatUntil 2",
                Syntax.anyChar.repeatUntil(Syntax.string("!!!", ())).string,
                "abc!!!"
              )(isRight(equalTo("abc!!!")))
            ),
            suite("optional")(
              parserTest("optional on empty", Syntax.anyChar.optional, "")(
                isRight(isNone)
              ),
              parserTest(
                "optional on mismatch",
                Syntax.char('a', "not a").optional,
                "b"
              )(
                isRight(isNone)
              ),
              parserTest("optional consumes", Syntax.anyChar.?, "a")(
                isRight(isSome(equalTo('a')))
              ),
              parserTest(
                "optional backtracks with auto-backtrack",
                ((Syntax.anyChar ~ Syntax
                  .char('a', "not a")).optional ~ Syntax.anyChar).autoBacktracking,
                "xy"
              )(isRight(equalTo((None, 'x'))))
            ),
            suite("Name")(
              parserTest(
                "name passed in failure",
                Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a") ?? "A",
                "hello"
              )(
                isLeft(equalTo(ParserError.Failure(List("A"), 1, "not a")))
              ),
              parserTest(
                "name scoped in sequence",
                (Syntax.anyChar ?? "A") ~
                  (Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a") ?? "B"),
                "hello"
              )(isLeft(equalTo(ParserError.Failure(List("B"), 2, "not a")))),
              parserTest(
                "nested names",
                ((Syntax.anyChar ?? "A") ~
                  (Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a") ?? "B" ?? "C")) ?? "D",
                "hello"
              )(isLeft(equalTo(ParserError.Failure(List("B", "C", "D"), 2, "not a")))),
              parserTest(
                "named <> named",
                ((Syntax.char('a', "not a") ?? "A") <> (Syntax.char('b', "not b") ?? "B")) ?? "C",
                "c"
              )(
                isLeft(
                  equalTo(
                    ParserError.AllBranchesFailed(
                      ParserError.Failure(List("A", "C"), 0, "not a"),
                      ParserError.Failure(List("B", "C"), 0, "not b")
                    )
                  )
                )
              ),
              parserTest(
                "named <+> named",
                ((Syntax.char('a', "not a") ?? "A") <+> (Syntax
                  .char('b', "not b") ?? "B")) ?? "C",
                "c"
              )(
                isLeft(
                  equalTo(
                    ParserError.AllBranchesFailed(
                      ParserError.Failure(List("A", "C"), 0, "not a"),
                      ParserError.Failure(List("B", "C"), 0, "not b")
                    )
                  )
                )
              ),
              parserTest(
                "s? ~ s? ~ s",
                (Syntax.anyChar ?? "A").optional ~ (Syntax.anyChar ?? "B").optional ~ (Syntax
                  .char('c', "not c") ?? "C"),
                "abd"
              )(
                isLeft(equalTo(ParserError.Failure(List("C"), 2, "not c")))
              )
            ),
            suite("manual backtracking")(
              parserTest(
                "auto backtrack can be turned off for <+>",
                ((Syntax.char('a', "not a") ~
                  Syntax.char('b', "not b")) <+> Syntax.anyChar).manualBacktracking,
                "ac"
              )(
                isLeft(equalTo(ParserError.Failure(Nil, 1, "not b")))
              ),
              parserTest(
                "auto backtrack can be turned off for <>",
                ((Syntax.char('a', "not a") ~>
                  charB) <> Syntax.anyChar).manualBacktracking,
                "ac"
              )(
                isLeft(equalTo(ParserError.Failure(Nil, 1, "not b")))
              ),
              parserTest(
                "manual backtrack works with <+>",
                ((Syntax.char('a', "not a") ~
                  Syntax.char(
                    'b',
                    "not b"
                  )).backtrack <+> Syntax.anyChar).manualBacktracking,
                "ac"
              )(
                isRight(isRight(equalTo('a')))
              ),
              parserTest(
                "manual backtrack works with <>",
                ((Syntax.char('a', "not a") ~>
                  Syntax
                    .char(
                      'b',
                      "not b"
                    )
                    .as('b')).backtrack <> Syntax.anyChar).manualBacktracking,
                "ac"
              )(
                isRight(equalTo('a'))
              ),
              parserTest(
                "optional with manual backtrack",
                ((Syntax.anyChar ~ Syntax
                  .char('a', "not a")).backtrack.optional ~ Syntax.anyChar).manualBacktracking,
                "xy"
              )(isRight(equalTo((None, 'x')))),
              parserTest(
                "optional with backtrack off",
                ((Syntax.anyChar ~ Syntax
                  .char('a', "not a")).optional ~ Syntax.anyChar).manualBacktracking,
                "xy"
              )(isLeft(equalTo(ParserError.Failure(Nil, 1, "not a"))))
            ),
            suite("Regex base constructors")(
              parserTest(
                "digit, passing",
                Syntax.digit,
                "1"
              )(isRight(equalTo('1'))),
              parserTest(
                "digits, passing",
                Syntax.digit.repeat.string,
                "12345"
              )(isRight(equalTo("12345"))),
              parserTest(
                "digits ~ letters",
                Syntax.digit.repeat.string ~ Syntax.letter.repeat.string,
                "12345abcd"
              )(isRight(equalTo(("12345", "abcd")))),
              parserTest("digits, passing", Syntax.digit.+.string, "123abc")(
                isRight(equalTo("123"))
              ),
              parserTest("digits, failing", Syntax.digit.+.string, "abc123")(
                isLeft(equalTo(ParserError.UnexpectedEndOfInput))
              )
            ),
            parserTest("Not, inner failing", Syntax.string("hello", ()).not("it was hello"), "world")(
              isRight(equalTo(()))
            ),
            parserTest("Not, inner passing", Syntax.string("hello", ()).not("it was hello"), "hello")(
              isLeft(equalTo(ParserError.Failure(Nil, 5, "it was hello")))
            ),
            parserTest(
              "of",
              (Syntax.alphaNumeric.repeat.string ~ Syntax.string("->", ()) ~ Syntax.digit.repeat.string
                .transform(_.toInt, (n: Int) => n.toString)).of[TestCaseClass],
              "hello->123"
            )(
              isRight(equalTo(TestCaseClass("hello", 123)))
            )
          ),
          suite("Parser only")(
            parserTest_(
              "flatMap, passing",
              (for {
                ch1 <- Parser.anyChar
                ch2 <- Parser.anyChar
              } yield (ch1, ch2)),
              "he"
            )(isRight(equalTo(('h', 'e'))))
          ),
          parserTest_(
            "to",
            (Parser.alphaNumeric.repeat.string ~
              Parser.string("->", ()) ~
              Parser.digit.repeat.string.map(_.toInt)).to[TestCaseClass],
            "hello->123"
          )(
            isRight(equalTo(TestCaseClass("hello", 123)))
          )
        )
      }: _*
    )
  }

  private def createParserTest[E, T](
      implementation: ParserImplementation
  )(name: String, syntax: Syntax[E, Char, Char, T, T], input: String)(
      assertion: Assertion[Either[ParserError[E], T]]
  ): ZSpec[Any, Nothing] =
    test(name) {
//      Debug.printParserTree(syntax.asParser.optimized)
      assert(syntax.parseString(input, implementation))(assertion)
    }

  private def createParserTest_[E, T](
      implementation: ParserImplementation
  )(name: String, parser: Parser[E, Char, T], input: String)(
      assertion: Assertion[Either[ParserError[E], T]]
  ): ZSpec[Any, Nothing] =
    test(name)(
      assert(parser.parseString(input, implementation))(assertion)
    )
//    test(name)(assert(syntax.parseString(input))(assertion))
}
