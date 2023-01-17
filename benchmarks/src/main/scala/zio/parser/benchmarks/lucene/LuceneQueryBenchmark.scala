package zio.parser.benchmarks.lucene

import cats.parse.Parser
import org.openjdk.jmh.annotations.{
  Benchmark,
  BenchmarkMode,
  Fork,
  Measurement,
  Mode,
  OutputTimeUnit,
  Scope,
  Setup,
  State,
  Warmup
}
import zio.Chunk
import zio.parser.{ParserImplementation, Syntax}
import zio.parser.Parser.ParserError
import zio.parser.internal.Debug

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class LuceneQueryBenchmark {
  var testQuery: String                                         = _
  var testQueryChunk: Chunk[Char]                               = _
  var catsParser: CatsLuceneQueryParser                         = _
  var zioParserQuery: Syntax[String, Char, Char, Query]         = _
  var zioParserStrippedQuery: Syntax[String, Char, Char, Query] = _

  @Setup
  def setUp(): Unit = {
    testQuery =
      "status:(active OR pending) AND title:(full text search)^2 AND date:[2012-01-01 TO 2012-12-31] AND (quikc~ brwn~ foks~)"
    testQueryChunk = Chunk.fromArray(testQuery.toCharArray)

    catsParser = new CatsLuceneQueryParser()
    val zioParser = new ZioLuceneQueryParser()
    zioParserQuery = zioParser.query
    zioParserStrippedQuery = zioParser.query.strip
  }

  @Benchmark
  def catsParse(): Either[Parser.Error, Query] =
    catsParser.query.parseAll(testQuery)

  @Benchmark
  def zioParse(): Either[ParserError[String], Query] =
    zioParserQuery.parseString(testQuery)

  @Benchmark
  def zioParseStrippedRecursive(): Either[ParserError[String], Query] =
    zioParserStrippedQuery.parseString(testQuery, ParserImplementation.Recursive)

  @Benchmark
  def zioParseStrippedRecursiveChunk(): Either[ParserError[String], Query] =
    zioParserStrippedQuery.parseChunk(testQueryChunk)

  @Benchmark
  def zioParseStrippedOpStack(): Either[ParserError[String], Query] =
    zioParserStrippedQuery.parseString(testQuery, ParserImplementation.StackSafe)

//  @Benchmark
//  def zioParseStrippedVM(): Either[ParserError[String], Query] =
//    zioParserStrippedQuery.parseChars(testQueryChunk, ParserImplementation.VM)
}

//object LuceneQueryBenchmark extends LuceneQueryBenchmark {
//  def main(args: Array[String]): Unit = {
//    setUp()
//    Debug.printParserTree(zioParserQuery.asParser.optimized)
//    println("----")
//    Debug.printParserTree(zioParserStrippedQuery.asParser.optimized)
//    println(s"ZIO Parser result: ${zioParse()}")
//    println(s"ZIO Parser stripped result: ${zioParseStrippedRecursive()}")
//    println(s"ZIO Parser stripped op-stack result: ${zioParseStrippedOpStack()}")
//    println(s"Cats Parser result: ${catsParse()}")
//
////    val builder = new VMBuilder
////    builder.compile(zioParserQuery.asParser.asInstanceOf[ErasedParser])
////    println(builder.result())
//  }
//}
