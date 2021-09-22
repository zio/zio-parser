package zio.parser.benchmarks

import zio.parser.internal.Debug

abstract class ParserBenchmarkTestRunner[T, B <: ParserBenchmark[T]] {
  val self: ParserBenchmark[T]

  def resultToString(value: T): String =
    value.toString

  def main(args: Array[String]): Unit = {
    self.setUp()
    println(s"input: ${self.value}")
    println()
//    Console.in.readLine()
//    while(true) self.zioParserOnString()
//    Debug.printSyntaxTree(self.zioSyntax)
//    println("---")
    Debug.printParserTree(self.zioSyntax.asParser.optimized)
//
//    val builder = new VMBuilder
//    builder.add(self.zioSyntax.optimized.asInstanceOf[ErasedSyntax])
//    println(builder.result())
//
    println(s"Cats Parser result: ${self.catsParse().map(resultToString)}")
//    println(s"ZIO Parser result: ${self.zioParserRecursiveOnChunk().map(resultToString)}")
//    println(s"ZIO Parser result: ${self.zioParserRecursiveUnitOnChunk().map(resultToString)}")
    println(s"ZIO Parser result:  ${self.zioParserOpStack().map(resultToString)}")
    println(s"ZIO Parser result:  ${self.zioParserRecursive().map(resultToString)}")
//    Console.in.readLine()
//    println(s"Cats Parse result: ${self.catsParse().map(resultToString)}")
//    println(
//      s"Fastparse result:  ${self.fastParse().fold((s, n, e) => s"Left(${(s, n, e)})", (v, _) => s"Right(${resultToString(v)})")}"
//    )
//    println(s"Atto result:       ${self.attoParse().either.map(resultToString)}")
//    println(s"Parboiled result:  ${self.parboiledParse().toEither.map(resultToString)}")
//    println(s"Parsley result:    ${self.parsleyParse().toEither.map(resultToString)}")
//    println(s"Parserz result:    ${self.parserzParse()._2.map(t => resultToString(t._2))}")
  }
}
