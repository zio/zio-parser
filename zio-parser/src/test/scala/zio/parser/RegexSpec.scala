package zio.parser

import zio.Chunk
import zio.random.Random
import zio.random
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object RegexSpec extends DefaultRunnableSpec {
  val keywordStrings =
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

  val keywordsChars = keywordStrings.map(_.toCharArray())

  val keywordsRegex = keywordStrings.map(Regex.string(_)).reduce(_ | _)

  val fooRegex = Regex.Tabular(Regex.string("foo"))

  val fooOrBar = Regex.Tabular(Regex.string("foo") | Regex.string("bar"))

  val aOrB = Regex.Tabular(Regex.string("a") | Regex.string("b"))

  val keywords = Regex.Tabular(keywordsRegex)

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("RegexSpec")(
      suite("string")(
        test("positive matches") {
          assert(fooRegex.matches("foo"))(isTrue)
        },
        test("negative matches") {
          assert(fooRegex.matches("bar"))(isFalse)
        },
        test("single-letter positive or") {
          assert(aOrB.matches("a"))(isTrue) &&
          assert(aOrB.matches("b"))(isTrue)
        },
        test("word positive or") {
          assert(fooOrBar.matches("foo"))(isTrue) &&
          assert(fooOrBar.matches("bar"))(isTrue)
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
        singleChar("literal 1", Regex.string("X"), _ == 'X', Gen.anyASCIIChar)
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
      testM("literal0") {
        val r = Regex.string("").compile
        check(Gen.anyString) { input =>
          assert(r.test(0, input))(equalTo(0))
        }
      },
      testM("literal+") {
        check(Gen.string1(Gen.anyUnicodeChar)) { str =>
          val r = Regex.string(str).compile

          val bad = "not" + str
          assert(r.test(0, str))(equalTo(str.length)) &&
          assert(r.test(0, bad))(isLessThan(str.length))
        }
      },
      suite("combinators")(
        testM("string ~ string") {
          check(Gen.string1(Gen.anyUnicodeChar), Gen.string1(Gen.anyUnicodeChar)) { (s1, s2) =>
            val r = (Regex.string(s1) ~ Regex.string(s2)).compile
            assert(r.test(0, s1 ++ s2))(equalTo(s1.length + s2.length))
          }
        },
        test("charIn & charIn") {
          val r = (Regex.charIn('a', 'b', 'c') & Regex.charIn('b')).compile

          assert(r.test(0, "a"))(equalTo(Regex.NotMatched)) &&
          assert(r.test(0, "b"))(equalTo(1))
        },
        testM("string | string") {
          check(Gen.string1(Gen.anyUnicodeChar), Gen.string1(Gen.anyUnicodeChar)) { (s1, s2) =>
            val r = (Regex.string(s1) | Regex.string(s2)).compile
            (assert(r.test(0, s1))(equalTo(s1.length)) &&
            assert(r.test(0, s2))(equalTo(s2.length)))
          }
        },
        testM("atLeast") {
          check(Gen.chunkOfBounded(1, 20)(Gen.numericChar), Gen.int(0, 20)) { (chunk, min) =>
            val r        = Regex.anyDigit.atLeast(min).compile
            val expected = if (chunk.length >= min) chunk.length else Regex.NeedMoreInput
            assert(r.test(0, new String(chunk.toArray)))(equalTo(expected))
          }
        },
        testM("atMost") {
          check(Gen.int(0, 20), Gen.int(0, 20)) { (len, max) =>
            val s = "abc" * len
            val r = Regex.string("abc").atMost(max).compile

            val expected = math.min(len, max)
            assert(r.test(0, s))(equalTo(expected))
          }
        } @@ TestAspect.ignore, // TODO
        testM("between") {
          check(Gen.int(0, 20), Gen.int(0, 20), Gen.int(0, 20)) { (len, a, b) =>
            val max = Math.max(a, b)
            val min = Math.min(a, b)
            val s   = "x" * len
            val r   = Regex.string("x").between(min, max).compile

            val expected = if (len >= min) Math.min(len, max) else Regex.NeedMoreInput
            assert(r.test(0, s))(equalTo(expected))
          }
        } @@ TestAspect.ignore  // TODO
      ),
      suite("end of stream")(
        test("oneOf(a, b)") {
          assert(
            Regex.charIn('a', 'b').compile.test(0, "")
          )(equalTo(Regex.NeedMoreInput))
        },
        test("oneOf(a)") {
          assert(
            Regex.char('a').compile.test(0, "")
          )(equalTo(Regex.NeedMoreInput))
        },
        test("anyChar.atLeast(0)") {
          assert(
            Regex.anyChar.atLeast(0).compile.test(0, "")
          )(equalTo(0))
        },
        test("char(x).atLeast(0)") {
          assert(
            Regex.char('x').atLeast(0).compile.test(0, "")
          )(equalTo(0))
        }
      ),
      test("test") {
        assert("""^\x00\x00\xff""".r.pattern.matcher("\u0000\u0000\u00ff").find(0))(isTrue)
      }
    )

  private def singleChar(name: String, r: Regex, test: Char => Boolean, gen: Gen[Random, Char] = Gen.anyChar) =
    testM(name) {
      val compiled = r.compile
      check(gen) { ch =>
        assert(compiled.test(0, ch.toString) == 1)(equalTo(test(ch)))
      }
    }

  private def multiCharPassing(name: String, r: Regex, gen: Gen[Random, Char]) =
    testM(name) {
      val compiled = r.compile
      check(Gen.chunkOf1(gen)) { input =>
        assert(compiled.test(0, new String(input.toArray)))(equalTo(input.length))
      }
    }

  private def multiCharFailing(name: String, r: Regex, gen: Gen[Random, Char], counterexample: Char) =
    testM(name) {
      val compiled = r.compile
      checkM(Gen.chunkOf1(gen)) { input =>
        random.nextIntBetween(0, input.length).map { injectionPoint =>
          val injected = (input.take(injectionPoint) :+ counterexample) ++ input.drop(injectionPoint)
          val expected =
            if (injectionPoint == 0)
              Regex.NeedMoreInput
            else
              injectionPoint
          val result   = compiled.test(0, new String(injected.toArray))
          assert(result)(equalTo(expected))
        }
      }
    }
}
