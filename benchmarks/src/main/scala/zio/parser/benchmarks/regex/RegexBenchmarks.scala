package zio.parser.benchmarks.regex

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
import zio.parser.Regex

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 3, time = 3, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 2, timeUnit = TimeUnit.SECONDS)
@Fork(value = 2)
class RegexBenchmarks {

  var value: String               = _
  var valueChunk: Chunk[Char]     = _
  var anyCharRep: Regex.Compiled  = _
  var someCharRep: Regex.Compiled = _
  var literal: Regex.Compiled     = _
  var literalRep: Regex.Compiled  = _

  @Setup
  def setUp(): Unit = {
    value = "hello" * 1000
    valueChunk = Chunk.fromArray(value.toCharArray)
    anyCharRep = Regex.anyChar.atLeast(0).compile
    someCharRep = Regex.charIn('h', 'e', 'l', 'o').atLeast(0).compile
    literal = Regex.string("hello").compile
    literalRep = Regex.string("hello").atLeast(0).compile
  }

  @Benchmark
  def anyCharRepeated(): Int =
    anyCharRep.test(0, value) // TODO: test vs passthrough

  @Benchmark
  def someCharRepeated(): Int =
    someCharRep.test(0, value)

  @Benchmark
  def literalString(): Int =
    literal.test(0, value) // TODO: test vs regionMatches

  @Benchmark
  def literalStringRepeated(): Int =
    literalRep.test(0, value)
}
