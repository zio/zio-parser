package zio.parser.benchmarks.micro

import org.openjdk.jmh.annotations._
import zio.Chunk
import zio.parser.Regex

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(value = 2)
class Experiments {

  val longString: String     = ("hello" * 1000) + "world" + ("hello" * 1000) + "!!!"
  val longChunk: Chunk[Char] = Chunk.fromIterable(longString.toCharArray)

  val regexClass   = Regex.charIn('h', 'e', 'l', 'o').atLeast(1).compile
  val builtInRegex = """\G[helo]+""".r

  val regex2Class   = Regex.string("hello").compile
  val builtInRegex2 = """\Ghello""".r

  @Setup
  def setup(): Unit = {}

  @Benchmark
  def shortSliceString(): String =
    longString.slice(5000, 5006)

  @Benchmark
  def shortSliceChunk1(): Chunk[Char] =
    longChunk.drop(5000).take(5)

  @Benchmark
  def shortSliceChunk2(): Chunk[Char] =
    longChunk.slice(5000, 5006)

  @Benchmark
  def longSliceString(): String =
    longString.slice(2000, 5006)

  @Benchmark
  def longSliceChunk1(): Chunk[Char] =
    longChunk.drop(2000).take(3005)

  @Benchmark
  def longSliceChunk2(): Chunk[Char] =
    longChunk.slice(2000, 5006)

  @Benchmark
  def shortSliceStringS(): String =
    longString.slice(5000, 5006)

  @Benchmark
  def shortSliceChunk1S(): String =
    String.copyValueOf(longChunk.drop(5000).take(5).toArray)

  @Benchmark
  def shortSliceChunk2S(): String =
    new String(longChunk.slice(5000, 5006).toArray)

  @Benchmark
  def longSliceStringS(): String =
    longString.slice(2000, 5006)

  @Benchmark
  def longSliceChunk1S(): String =
    String.copyValueOf(longChunk.drop(2000).take(3005).toArray)

  @Benchmark
  def longSliceChunk2S(): String =
    new String(longChunk.drop(2000).take(3005).toArray)

  @Benchmark
  def getCharString: Char =
    longString(5000)

  @Benchmark
  def getCharChunk: Char =
    longChunk(5000)

  @Benchmark
  def regexOnString =
    regexClass.test(0, longString)

  @Benchmark
  def builtInRegexOnString =
    builtInRegex.findPrefixMatchOf(longString) match {
      case Some(m) => m.end
      case None    => 0
    }

  @Benchmark
  def regexOnStringOffset =
    regexClass.test(2000, longString)

  @Benchmark
  def builtInRegexOnStringOffset =
    builtInRegex.findPrefixMatchOf(longString.slice(2000, longString.length)) match {
      case Some(m) => m.end
      case None    => 0
    }

  @Benchmark
  def builtInRegexOnStringOffset2 = {
    val matcher = builtInRegex.pattern.matcher(longString)
    if (matcher.find(2000))
      matcher.end()
    else {
      Regex.NotMatched
    }
  }

  @Benchmark
  def regex2OnStringOffset =
    regex2Class.test(2000, longString)

  @Benchmark
  def builtInRegex2OnStringOffset2 = {
    val matcher = builtInRegex2.pattern.matcher(longString)
    if (matcher.find(2000))
      matcher.end()
    else {
      Regex.NotMatched
    }
  }

}
