package zio.parser.benchmarks.micro

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
import zio.parser.Parser.ParserError
import zio.parser.{Regex, Syntax}

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1)
class CharParserMicroBenchmarks {

  type String10 = (String, String, String, String, String, String, String, String, String, String)
  var skipAndTransformSyntax: Syntax[Nothing, Char, Char, String, String]                    = _
  var skipAndTransformOrElseSyntax: Syntax[String, Char, Char, String, String]               = _
  var skipAndTransformZipSyntax: Syntax[String, Char, Char, String10, String10]              = _
  var skipAndTransformRepeatSyntax: Syntax[String, Char, Char, Chunk[String], Chunk[String]] = _
  var repeatWithSep0Syntax: Syntax[String, Char, Char, Chunk[String], Chunk[String]]         = _
  var hello: Chunk[Char]                                                                     = _
  var hellos: Chunk[Char]                                                                    = _
  var world: Chunk[Char]                                                                     = _
  var hellosSep: String                                                                      = _

  @Setup
  def setUp(): Unit = {
    hello = Chunk('h', 'e', 'l', 'l', 'o')
    world = Chunk('w', 'o', 'r', 'l', 'd')
    hellos = Chunk.fromArray(("hello" * 10).toCharArray)
    hellosSep = (1 to 10000).map(_ => "hello").mkString(",")

    skipAndTransformSyntax = Syntax
      .unsafeRegexDiscard(Regex.anyChar.atLeast(0), hello)
      .transform(_ => "hello", _ => ())
    val _ = skipAndTransformSyntax.asParser.optimized

    val literalHello =
      Syntax.regexDiscard(Regex.string("hello"), "not hello", hello).transform[String, String](_ => "hello", _ => ())

    skipAndTransformOrElseSyntax = literalHello |
      Syntax.regexDiscard(Regex.string("world"), "not world", world).transform[String, String](_ => "world", _ => ())
    val _ = skipAndTransformOrElseSyntax.asParser.optimized

    skipAndTransformRepeatSyntax = literalHello.repeat
    val _ = skipAndTransformRepeatSyntax.asParser.optimized

    skipAndTransformZipSyntax =
      literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello ~ literalHello
    val _ = skipAndTransformZipSyntax.asParser.optimized

    repeatWithSep0Syntax = Syntax.string("hello", "hello").repeatWithSep0(Syntax.char(','))
    val _ = repeatWithSep0Syntax.asParser.optimized
  }

  @Benchmark
  def skipAndTransform(): Either[ParserError[Nothing], String] =
    skipAndTransformSyntax.parseChars(hello)

  @Benchmark
  def skipAndTransformOrElse(): Either[ParserError[String], String] =
    skipAndTransformOrElseSyntax.parseChars(world)

  @Benchmark
  def skipAndTransformRepeat(): Either[ParserError[String], Chunk[String]] =
    skipAndTransformRepeatSyntax.parseChars(hellos)

  @Benchmark
  def skipAndTransformZip(): Either[ParserError[String], String10] =
    skipAndTransformZipSyntax.parseChars(hellos)

  @Benchmark
  def repeatWithSep0(): Either[ParserError[String], Chunk[String]] =
    repeatWithSep0Syntax.parseString(hellosSep)
}

object CharParserMicroBenchmarks extends CharParserMicroBenchmarks {
  def main(args: Array[String]): Unit = {
    setUp()
//    println(skipAndTransform())
//    println(skipAndTransformOrElse())
//    println(skipAndTransformRepeat())
//    println(skipAndTransformZip())
    println(repeatWithSep0().map(_.length))
  }
}
