package zio.parser.benchmarks.basic

import zio.Chunk
import zio.parser.benchmarks.{ParserBenchmark, ParserBenchmarkTestRunner, Parserz}
import zio.parser.internal.Debug

import scala.util.{Random, Try}

class Zipping extends ParserBenchmark[Zips] {
  override def loadInput(): String = {
    val N  = 1000
    val sb = new StringBuilder
    for (_ <- 1 to N) {
      val a = "A" + Random.alphanumeric.take(5).mkString
      val b = "B" + Random.alphanumeric.take(5).mkString
      val c = "C" + Random.alphanumeric.take(5).mkString
      val d = "D" + Random.alphanumeric.take(5).mkString
      sb.append(s"$a,$b,$c,$d\n")
    }
    sb.toString()
  }

  override final val zioSyntax: zio.parser.Syntax[String, Char, Char, Zips, Zips] = {
    import zio.parser._

    val item  = Syntax
      .charNotIn(',', '\n')
      .repeat
      .string
    val sep   = Syntax.char(',')
    val tuple = (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ item
    val zip   = tuple.transform(
      { case (as, bs, cs, ds) => Zip(as, bs, cs, ds) },
      (zip: Zip) => (zip.a, zip.b, zip.c, zip.d)
    )
    val line  = zip <~ Syntax.char('\n')
    val lines = line.repeat0.transform[Zips, Zips](Zips.apply, z => Chunk.fromIterable(z.zips)).manualBacktracking
    lines
  }

  override final val catParser: cats.parse.Parser0[Zips] = {
    import cats.parse._

    val item  = Parser.charsWhile(ch => ch != ',' && ch != '\n')
    val sep   = Parser.char(',')
    val tuple = (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ item
    val zip   = tuple.map { case (((as, bs), cs), ds) => Zip(as, bs, cs, ds) }
    val line  = zip <* Parser.char('\n')
    val lines = line.rep0.map(lst => Zips(lst))
    lines
  }

  // TODO: implement for all

  override def fastParseP[P: fastparse.P]: fastparse.P[Zips] = ???

  override val attoParser: atto.Parser[Zips] = null

  override def runParboiledParser(input: String): Try[Zips] = ???

  override val parsley: org.http4s.parsley.Parsley[Zips]            = null
  override val parserz: Parserz.Grammar[Any, Nothing, String, Zips] = null
}

case class Zip(a: String, b: String, c: String, d: String)
case class Zips(zips: Seq[Zip])

object Zipping extends ParserBenchmarkTestRunner[Zips, Zipping] {
  override val self: ParserBenchmark[Zips] = new Zipping
}
