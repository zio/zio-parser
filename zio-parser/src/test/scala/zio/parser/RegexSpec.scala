package zio.parser

import zio._
import zio.test.Assertion._
import zio.test._

import scala.annotation.nowarn

object RegexSpec extends DefaultRunnableSpec {
  val keywordStrings: List[String] =
    List(
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "forSome",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "this",
      "throw",
      "trait",
      "try",
      "true",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield"
    )

  val keywordsChars: List[Array[Char]] = keywordStrings.map(_.toCharArray())

  val keywordsRegex: Regex = keywordStrings.map(Regex.string(_)).reduce(_ | _)

  val fooRegex: Regex.Tabular.Tabular = Regex.Tabular(Regex.string("foo"))

  val fooOrBar: Regex.Tabular.Tabular = Regex.Tabular(Regex.string("foo") | Regex.string("bar"))

  val aOrB: Regex.Tabular.Tabular = Regex.Tabular(Regex.string("a") | Regex.string("b"))

  val keywords: Regex.Tabular.Tabular = Regex.Tabular(keywordsRegex)

  override def spec: ZSpec[Environment, Any] =
    suite("RegexSpec")(
      suite("string")(
        test("positive matches") {
          assertTrue(fooRegex.matches("foo"))
        },
        test("negative matches") {
          assertTrue(!(fooRegex.matches("bar")))
        },
        test("single-letter positive or") {
          assertTrue(aOrB.matches("a")) &&
          assertTrue(aOrB.matches("b"))
        },
        test("word positive or") {
          assertTrue(fooOrBar.matches("foo")) &&
          assertTrue(fooOrBar.matches("bar"))
        }
      ),
      suite("keywords") {
        test("matches all keywords") {
          assert(keywordStrings.map(keywords.matches(_)))(Assertion.forall(Assertion.isTrue))
        }
      },
      suite("digits")(
        test("matches all digits") {
          assert(Regex.digits.compile.matches("123"))(Assertion.isTrue)
        },
        test("matches the first few digits") {
          assert(Regex.digits.compile.matches("123ABC"))(Assertion.isTrue)
        }
      ),
      suite("single-char constructors")(
        singleChar("alphaNumeric", Regex.anyAlphaNumeric, _.isLetterOrDigit),
        singleChar("digit", Regex.anyDigit, _.isDigit),
        singleChar("letter", Regex.anyLetter, _.isLetter),
        singleChar("whitespace", Regex.whitespace, _.isWhitespace),
        singleChar(
          "filter",
          Regex.filter(ch => ch == 'a' || ch == 'b'),
          ch => ch == 'a' || ch == 'b',
          Gen.fromIterable(List('a', 'b', 'c', 'd'))
        ),
        singleChar(
          "oneOf",
          Regex.charIn('a', 'b'),
          ch => ch == 'a' || ch == 'b',
          Gen.fromIterable(List('a', 'b', 'c', 'd'))
        ),
        singleChar(
          "noneOf",
          Regex.charNotIn('a', 'b'),
          ch => ch != 'a' && ch != 'b',
          Gen.fromIterable(List('a', 'b', 'c', 'd'))
        ),
        singleChar("literal 1", Regex.string("X"), _ == 'X', Gen.asciiChar)
      ),
      suite("multi-char constructors passing")(
        multiCharPassing("alphaNumerics", Regex.alphaNumerics, Gen.alphaNumericChar),
        multiCharPassing("digits", Regex.digits, Gen.numericChar),
        multiCharPassing("letters", Regex.letters, Gen.alphaChar),
        multiCharPassing("whitespaces", Regex.whitespace, Gen.fromIterable(List(' ', '\n', '\t')))
      ),
      suite("multi-char constructors failing")(
        multiCharFailing("alphaNumerics", Regex.alphaNumerics, Gen.alphaNumericChar, '!'),
        multiCharFailing("digits", Regex.digits, Gen.numericChar, 'a'),
        multiCharFailing("letters", Regex.letters, Gen.alphaChar, '0')
      ),
      test("literal0") {
        val r = Regex.string("").compile
        check(Gen.string) { input =>
          assertTrue(r.test(0, input) == 0)
        }
      },
      test("literal+") {
        check(Gen.string1(Gen.unicodeChar)) { str =>
          val r = Regex.string(str).compile

          val bad = "not" + str
          assertTrue(r.test(0, str) == str.length) &&
          assertTrue(r.test(0, bad) < str.length)
        }
      },
      suite("combinators")(
        test("string ~ string") {
          check(Gen.string1(Gen.unicodeChar), Gen.string1(Gen.unicodeChar)) { (s1, s2) =>
            val r = (Regex.string(s1) ~ Regex.string(s2)).compile
            assertTrue(r.test(0, s1 ++ s2) == s1.length + s2.length)
          }
        },
        test("charIn & charIn") {
          val r = (Regex.charIn('a', 'b', 'c') & Regex.charIn('b')).compile

          assertTrue(r.test(0, "a") == Regex.NotMatched) &&
          assertTrue(r.test(0, "b") == 1)
        },
        test("string | string") {
          check(Gen.string1(Gen.unicodeChar), Gen.string1(Gen.unicodeChar)) { (s1, s2) =>
            val r = (Regex.string(s1) | Regex.string(s2)).compile
            (assertTrue(r.test(0, s1) == s1.length) &&
            assertTrue(r.test(0, s2) == s2.length))
          }
        },
        test("atLeast") {
          check(Gen.chunkOfBounded(1, 20)(Gen.numericChar), Gen.int(0, 20)) { (chunk, min) =>
            val r        = Regex.anyDigit.atLeast(min).compile
            val expected = if (chunk.length >= min) chunk.length else Regex.NeedMoreInput
            assertTrue(r.test(0, new String(chunk.toArray)) == expected)
          }
        },
        test("atMost") {
          check(Gen.int(0, 20), Gen.int(0, 20)) { (len, max) =>
            val s = "abc" * len
            val r = Regex.string("abc").atMost(max).compile

            val expected = math.min(len, max)
            assertTrue(r.test(0, s) == expected)
          }
        } @@ TestAspect.ignore, // TODO
        test("between") {
          check(Gen.int(0, 20), Gen.int(0, 20), Gen.int(0, 20)) { (len, a, b) =>
            val max = Math.max(a, b)
            val min = Math.min(a, b)
            val s   = "x" * len
            val r   = Regex.string("x").between(min, max).compile

            val expected = if (len >= min) Math.min(len, max) else Regex.NeedMoreInput
            assertTrue(r.test(0, s) == expected)
          }
        } @@ TestAspect.ignore  // TODO
      ),
      suite("end of stream")(
        test("oneOf(a, b)") {
          assertTrue(Regex.charIn('a', 'b').compile.test(0, "") == Regex.NeedMoreInput)
        },
        test("oneOf(a)") {
          assertTrue(Regex.char('a').compile.test(0, "") == Regex.NeedMoreInput)
        },
        test("anyChar.atLeast(0)") {
          assertTrue(Regex.anyChar.atLeast(0).compile.test(0, "") == 0)
        },
        test("char(x).atLeast(0)") {
          assertTrue(Regex.char('x').atLeast(0).compile.test(0, "") == 0)
        }
      ),
      test("test") {
        assert("""^\x00\x00\xff""".r.pattern.matcher("\u0000\u0000\u00ff").find(0))(isTrue)
      }
    )

  @nowarn private def singleChar(name: String, r: Regex, test: Char => Boolean, gen: Gen[Random, Char] = Gen.char) =
    testM(name) {
      val compiled = r.compile
      check(gen) { ch =>
        assertTrue(compiled.test(0, ch.toString) == 1 == test(ch))
      }
    }

  @nowarn private def multiCharPassing(name: String, r: Regex, gen: Gen[Random, Char]) =
    testM(name) {
      val compiled = r.compile
      check(Gen.chunkOf1(gen)) { input =>
        assertTrue(compiled.test(0, new String(input.toArray)) == input.length)
      }
    }

  @nowarn private def multiCharFailing(name: String, r: Regex, gen: Gen[Random, Char], counterexample: Char) =
    testM(name) {
      val compiled = r.compile
      check(Gen.chunkOf1(gen)) { input =>
        Random.nextIntBetween(0, input.length).map { injectionPoint =>
          val injected = (input.take(injectionPoint) :+ counterexample) ++ input.drop(injectionPoint)
          val expected =
            if (injectionPoint == 0)
              Regex.NeedMoreInput
            else
              injectionPoint
          val result   = compiled.test(0, new String(injected.toArray))
          assertTrue(result == expected)
        }
      }
    }
}
