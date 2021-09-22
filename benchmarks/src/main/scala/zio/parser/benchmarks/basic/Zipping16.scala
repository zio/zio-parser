package zio.parser.benchmarks.basic

import zio.Chunk
import zio.parser.benchmarks.{ParserBenchmark, ParserBenchmarkTestRunner, Parserz}
import zio.parser.internal.Debug

import scala.util.{Random, Try}

class Zipping16 extends ParserBenchmark[Zips16] {
  override def loadInput(): String = {
    val N  = 250
    val sb = new StringBuilder
    for (_ <- 1 to N) {
      sb.append((1 to 16).map(_ => Random.alphanumeric.take(6).mkString).mkString(","))
      sb.append('\n')
    }
    sb.toString()
  }

  override final val zioSyntax: zio.parser.Syntax[String, Char, Char, Zips16, Zips16] = {
    import zio.parser._

    val item  = Syntax
      .charNotIn(',', '\n')
      .repeat
      .string
    val sep   = Syntax.char(',')
    val tuple =
      (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ (item <~ sep) ~ item
    val zip   = tuple.transform(
      { case (as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os, ps) =>
        Zip16(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os, ps)
      },
      (zip: Zip16) =>
        (
          zip.a,
          zip.b,
          zip.c,
          zip.d,
          zip.e,
          zip.f,
          zip.g,
          zip.h,
          zip.i,
          zip.j,
          zip.k,
          zip.l,
          zip.m,
          zip.n,
          zip.o,
          zip.p
        )
    )
    val line  = zip <~ Syntax.char('\n')
    val lines = line.repeat0.transform[Zips16, Zips16](Zips16.apply, z => Chunk.fromIterable(z.zips)).manualBacktracking
    lines
  }

  override final val catParser: cats.parse.Parser0[Zips16] = {
    import cats.parse._

    val item  = Parser.charsWhile(ch => ch != ',' && ch != '\n')
    val sep   = Parser.char(',')
    val tuple =
      (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ (item <* sep) ~ item
    val zip   = tuple.map {
      case (((((((((((((((as, bs), cs), ds), es), fs), gs), hs), is), js), ks), ls), ms), ns), os), ps) =>
        Zip16(as, bs, cs, ds, es, fs, gs, hs, is, js, ks, ls, ms, ns, os, ps)
    }
    val line  = zip <* Parser.char('\n')
    val lines = line.rep0.map(lst => Zips16(lst))
    lines
  }

  // TODO: implement for all

  override def fastParseP[_: fastparse.P]: fastparse.P[Zips16] = ???

  override val attoParser: atto.Parser[Zips16] = null

  override def runParboiledParser(input: String): Try[Zips16] = ???

  override val parsley: org.http4s.parsley.Parsley[Zips16]            = null
  override val parserz: Parserz.Grammar[Any, Nothing, String, Zips16] = null
}

case class Zip16(
    a: String,
    b: String,
    c: String,
    d: String,
    e: String,
    f: String,
    g: String,
    h: String,
    i: String,
    j: String,
    k: String,
    l: String,
    m: String,
    n: String,
    o: String,
    p: String
)
case class Zips16(zips: Seq[Zip16])

object Zipping16 extends ParserBenchmarkTestRunner[Zips16, Zipping16] {
  override val self: ParserBenchmark[Zips16] = new Zipping16
}
