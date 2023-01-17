package zio.parser.benchmarks

import org.openjdk.jmh.annotations._
import org.spartanz.parserz.\/
import zio.Chunk

import java.util.concurrent.TimeUnit
import scala.util.Try

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 2)
abstract class ParserBenchmark[T] {
  def loadInput(): String
  val zioSyntax: zio.parser.Syntax[String, Char, Char, T]
  val catParser: cats.parse.Parser0[T]
  def fastParseP[P: fastparse.P]: fastparse.P[T]
  val attoParser: atto.Parser[T]
  def runParboiledParser(input: String): Try[T]
  val parsley: org.http4s.parsley.Parsley[T]
  val parserz: Parserz.Grammar[Any, Nothing, String, T]

  var value: String = _
  var valueAsChunk: Chunk[Char] = _

  @Setup
  def setUp(): Unit = {
    value = loadInput()
    valueAsChunk = Chunk.fromArray(value.toCharArray)
    zioSyntax.parseString("")
  }

//  @Benchmark
//  def zioParserExperiment(): Either[String, T] = {
//    zioCombinator.parse(0, chunk)
//  }

//  @Benchmark
//  def zioParserOnString(): Either[zio.parser.Parser.ParserError[String], T] = {
//    import zio.parser._
//    import zio.parser.source._
//    zioSyntax.parseString(value)
//  }

  @Benchmark
  def zioParserRecursive(): Either[zio.parser.Parser.ParserError[String], T] = {
    import zio.parser._
    zioSyntax.parseString(value, ParserImplementation.Recursive)
  }

  @Benchmark
  def zioParserRecursiveOnChunk(): Either[zio.parser.Parser.ParserError[String], T] = {
    import zio.parser._
    zioSyntax.parseChunk(valueAsChunk)
  }

  @Benchmark
  def zioParserOpStack(): Either[zio.parser.Parser.ParserError[String], T] = {
    import zio.parser._
    zioSyntax.parseString(value, ParserImplementation.StackSafe)
  }

  @Benchmark
  def catsParse(): Either[cats.parse.Parser.Error, T] = {
    import cats.parse._
    catParser.parseAll(value)
  }

//  @Benchmark
//  def fastParse(): fastparse.Parsed[T] = {
//    import fastparse._
//    parse[T](value, fastParseP(_), verboseFailures = true)
//  }
//
//  @Benchmark
//  def attoParse(): atto.ParseResult[T] = {
//    import atto._
//    import Atto._
//    attoParser.parse(value).done
//  }
//
//  @Benchmark
//  def parboiledParse(): Try[T] = {
//    runParboiledParser(value)
//  }
//
//  @Benchmark
//  def parsleyParse(): org.http4s.parsley.Result[T] = {
//    import org.http4s.parsley._
//
//    runParserThreadSafe(parsley, value)
//  }
//
//  @Benchmark
//  def parserzParse(): (Any, \/[String, (List[Char], T)]) = {
//    Parserz.parser(parserz)((), value.toList)
//  }
}
