package zio.parser.benchmarks

import zio._
import zio.parser.ParserImplementation
import zio.parser.benchmarks.basic.{RepeatAnyChar, StringAlternatives, Zipping, Zipping16}
import zio.parser.benchmarks.json._
import zio.parser.benchmarks.lucene.LuceneQueryBenchmark
import zio.test.Assertion.{anything, isRight}
import zio.test._

/** Tests whether the benchmark examples run with success */
object BenchmarksSpec extends ZIOSpecDefault {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Benchmarks")(
      suite("basic")(
        testParserBenchmark("RepeatAnyChar", new RepeatAnyChar),
        testParserBenchmark("StringAlternatives", new StringAlternatives),
        testParserBenchmark("Zipping", new Zipping),
        testParserBenchmark("Zipping16", new Zipping16)
      ),
      suite("json")(
        testParserBenchmark("BarBench", new BarBench),
        testParserBenchmark("Qux2Bench", new Qux2Bench),
        testParserBenchmark("Bla25Bench", new Bla25Bench),
        testParserBenchmark(
          "CountriesBench",
          new CountriesBench
        ) @@ TestAspect.ignore, // TODO: figure out why parser fails
        testParserBenchmark("Ugh10kBench", new Ugh10kBench)
      ),
      suite("lucene")(
        test("stacksafe") {
          val benchmark = new LuceneQueryBenchmark
          benchmark.setUp()
          assert(benchmark.zioParseStrippedOpStack())(isRight(anything))
        },
        test("recursive") {
          val benchmark = new LuceneQueryBenchmark
          benchmark.setUp()
          assert(benchmark.zioParseStrippedRecursive())(isRight(anything))
        }
      )
    ) @@ TestAspect.timeout(90.seconds)

  private def testParserBenchmark[T](
      name: String,
      create: => ParserBenchmark[T]
  ): Spec[Any, TestFailure[Nothing], TestSuccess] = {
    val benchmark = create
    benchmark.setUp()

    suite(name)(
      test("stacksafe") {
        assert(benchmark.zioSyntax.parseString(benchmark.value, ParserImplementation.StackSafe))(isRight(anything))
      },
      test("recursive") {
        assert(benchmark.zioSyntax.parseString(benchmark.value, ParserImplementation.Recursive))(isRight(anything))
      }
    )
  }
}
