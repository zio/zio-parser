package zio.parser

import zio.Chunk
import zio.test.Assertion._
import zio.test._

object PrinterSpec extends ZIOSpecDefault {
  private val charA: Syntax[String, Char, Char, Char] =
    Syntax.charIn('a')

  private val charB: Syntax[String, Char, Char, Char] =
    Syntax.charIn('b')

  case class TestCaseClass(a: Char, b: Char)

  override def spec: Spec[Environment, Any] =
    suite("Printing")(
      suite("Invertible syntax")(
        printerTest("anyChar", Syntax.anyChar, 'x')(isRight(equalTo("x"))),
        printerTest("filtered char, passing", Syntax.anyChar.filter((ch: Char) => ch == 'h', "not h"), 'h')(
          isRight(equalTo("h"))
        ),
        printerTest("filtered char, failing", Syntax.anyChar.filter((ch: Char) => ch == 'h', "not h"), 'e')(
          isLeft(equalTo("not h"))
        ),
        printerTest("transform", Syntax.anyChar.transform(_.toInt, (v: Int) => v.toChar), 66)(isRight(equalTo("B"))),
        printerTest(
          "transformEither, failing",
          Syntax.anyChar.transformEither[String, Int](_ => Left("bad"), (_: Int) => Left("bad")),
          100
        )(
          isLeft(equalTo("bad"))
        ),
        printerTest("s ~ s", Syntax.anyChar ~ Syntax.anyChar, ('x', 'y'))(isRight(equalTo("xy"))),
        printerTest("s ~ s ~ s", Syntax.anyChar ~ Syntax.anyChar ~ Syntax.anyChar, ('x', 'y', 'z'))(
          isRight(equalTo("xyz"))
        ),
        printerTest(
          "s ~ s, failing left",
          Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a") ~ Syntax.anyChar,
          ('b', 'c')
        )(
          isLeft(equalTo("not a"))
        ),
        printerTest(
          "s ~ s, failing right",
          Syntax.anyChar ~ Syntax.anyChar.filter((ch: Char) => ch == 'a', "not a"),
          ('a', 'b')
        )(
          isLeft(equalTo("not a"))
        ),
        printerTest("s <* s", Syntax.anyChar <~ Syntax.anyChar.asPrinted((), '?'), 'x')(isRight(equalTo("x?"))),
        printerTest("s *> s", Syntax.anyChar.asPrinted((), '?') ~> Syntax.anyChar, 'x')(isRight(equalTo("?x"))),
        printerTest("s | s, left passing", charA | charB, 'a')(
          isRight(equalTo("a"))
        ),
        printerTest("s | s, right passing", charA | charB, 'b')(
          isRight(equalTo("b"))
        ),
        printerTest("s | s, failing", charA | charB, 'c')(
          isLeft(equalTo("Not the expected character (b)"))
        ), {
          val hello = Syntax.string("hello", 'h')
          val world = Syntax.string("world", 'w')
          val all   = Syntax.string("all", 'a')
          val a     = hello ~ world
          val b     = hello ~ all
          printerTest(
            "s | s, left failing inside",
            a | b,
            ('h', 'a')
          )(isRight(equalTo("helloall")))
        },
        printerTest("s <+> s, left passing", charA <+> charB, Left('a'))(
          isRight(equalTo("a"))
        ),
        printerTest(
          "s <+> s, right passing",
          charA <+> charB,
          Right('b')
        )(isRight(equalTo("b"))),
        printerTest("s <+> s, failing", charA <+> charB, Right('c'))(
          isLeft(equalTo("Not the expected character (b)"))
        ),
        printerTest(
          "s?, passing",
          (charA ~ charB).?,
          Some(('a', 'b'))
        )(
          isRight(equalTo("ab"))
        ),
        printerTest(
          "s?, not passing",
          (charA ~ charB).?,
          None
        )(
          isRight(equalTo(""))
        ),
        suite("repeat")(
          printerTest("repeat empty", charA.repeat, Chunk.empty)(
            isRight(equalTo(""))
          ),
          printerTest("repeat 1", charA.repeat, Chunk('a'))(
            isRight(equalTo("a"))
          ),
          printerTest("repeat 3", charA.repeat, Chunk('a', 'a', 'a'))(
            isRight(equalTo("aaa"))
          )
        ),
        suite("repeat0")(
          printerTest("repeat0 empty", charA.repeat0, Chunk.empty)(
            isRight(equalTo(""))
          ),
          printerTest("repeat0 1", charA.repeat0, Chunk('a'))(
            isRight(equalTo("a"))
          ),
          printerTest("repeat0 3", charA.repeat0, Chunk('a', 'a', 'a'))(
            isRight(equalTo("aaa"))
          )
        ),
        suite("repeatWithSep")(
          printerTest("repeatWithSep", Syntax.anyChar.repeatWithSep(Syntax.char('-')), Chunk('a', 'b', 'c'))(
            isRight(equalTo("a-b-c"))
          )
        ),
        printerTest_("from", ((charA ~ charB).asPrinter).from[TestCaseClass], TestCaseClass('a', 'b'))(
          isRight(equalTo("ab"))
        )
      )
    )

  private def printerTest[E, T](name: String, syntax: Syntax[E, Char, Char, T], input: T)(
      assertion: Assertion[Either[E, String]]
  ): Spec[Any, Nothing] =
    test(name)(assert(syntax.printString(input))(assertion))

  private def printerTest_[E, T](name: String, printer: Printer[E, Char, T], input: T)(
      assertion: Assertion[Either[E, String]]
  ): Spec[Any, Nothing] =
    test(name)(assert(printer.printString(input))(assertion))
}
