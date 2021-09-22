//package zio.parser.benchmarks
//
//import org.openjdk.jmh.annotations._
//import java.util.concurrent.TimeUnit
//
//import zio.parser.Regex
//
//@BenchmarkMode(Array(Mode.Throughput))
//@OutputTimeUnit(TimeUnit.MILLISECONDS)
//@State(Scope.Benchmark)
//@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
//@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
//@Fork(value = 1)
//class RegexBenchmark {
//  val keywordStrings =
//    List(
//      "abstract",
//      "case",
//      "catch",
//      "class",
//      "def",
//      "do",
//      "else",
//      "extends",
//      "false",
//      "final",
//      "finally",
//      "for",
//      "forSome",
//      "if",
//      "implicit",
//      "import",
//      "lazy",
//      "match",
//      "new",
//      "null",
//      "object",
//      "override",
//      "package",
//      "private",
//      "protected",
//      "return",
//      "sealed",
//      "super",
//      "this",
//      "throw",
//      "trait",
//      "try",
//      "true",
//      "type",
//      "val",
//      "var",
//      "while",
//      "with",
//      "yield"
//    )
//
//  val keywordsChars = keywordStrings
//
//  val keywordsRegex = keywordStrings.map(Regex.string(_)).reduce(_ | _)
//
//  val keywordsCompiled = keywordsRegex.compile
//
//  val firstKeywordChars = keywordsChars.head
//  val lastKeywordChars = keywordsChars.last
//
//  val shortLiteral = Regex.string("true").compile
//  val longLiteral = Regex.string("supercalifragilisticexpialidocious").compile
//
//  val charsFalse = "false"
//  val charsTrue = "true"
//
//  val charsUnsuper = "superman"
//  val charsSuper = "supercalifragilisticexpialidocious"
//
//  val shortLiteral2 = Regex.string("true") | Regex.string("false")
//
//  val anyChars = Regex.anyChar.atLeast(0).compile
//
//  val anyCharsN = Regex.anyChar.atMost(charsSuper.length).compile
//
//  @Benchmark
//  def shortLiteralNegativeBenchmark() = shortLiteral.matches(charsFalse)
//
//  @Benchmark
//  def shortLiteralPositiveBenchmark() = shortLiteral.matches(charsTrue)
//
//  @Benchmark
//  def longLiteralNegativeBenchmark() = longLiteral.matches(charsUnsuper)
//
//  @Benchmark
//  def longLiteralPositiveBenchmark() = longLiteral.matches(charsSuper)
//
//  @Benchmark
//  def shortLiteral2PositiveBenchmark() = shortLiteral.matches(charsFalse)
//
//  @Benchmark
//  def shortLiteral2NegativeBenchmark() = shortLiteral.matches(charsUnsuper)
//
//  @Benchmark
//  def keywordLastPositiveBenchmark() = keywordsCompiled.matches(lastKeywordChars)
//
//  @Benchmark
//  def keywordNegativeBenchmark() = keywordsCompiled.matches(charsSuper)
//
//  @Benchmark
//  def keywordFirstPositiveBenchmark() = keywordsCompiled.matches(firstKeywordChars)
//
//  @Benchmark
//  def anyCharsBenchmark() = anyChars.matches(charsSuper)
//
//  @Benchmark
//  def anyCharsNBenchmark() = anyCharsN.matches(charsSuper)
//}
