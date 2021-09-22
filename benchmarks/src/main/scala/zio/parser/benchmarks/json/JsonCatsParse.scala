package zio.parser.benchmarks.json

import cats.parse.{Numbers, Parser => P, Parser0 => P0}
import zio.Chunk

// Based on: https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/self.scala
object JsonCatsParse {
  private[this] val whitespace: P[Unit]    = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P0[Unit] = whitespace.rep0.void

  /** This doesn't have to be super fast (but is fairly fast) since we use it in places where speed won't matter:
    * feeding it into a program that will convert it to bosatsu structured data
    */
  val parser: P[Json] = P.recursive[Json] { recurse =>
    val pnull   = P.string("null").as(Json.Null)
    val bool    = P.string("true").as(Json.Bool(true)).orElse(P.string("false").as(Json.Bool(false)))
    val justStr = JsonStringUtil.escapedString('"')
    val str     = justStr.map(Json.Str)
    val num     = Numbers.jsonNumber.map(s => Json.Num(BigDecimal(s)))

    val listSep: P[Unit] =
      P.char(',').surroundedBy(whitespaces0).void

    def rep0[A](pa: P[A]): P0[List[A]] =
      pa.repSep0(listSep).surroundedBy(whitespaces0)

    val list = rep0(recurse).with1
      .between(P.char('['), P.char(']'))
      .map(vs => Json.Arr(Chunk.fromIterable(vs)))

    val kv: P[(String, Json)] =
      justStr ~ (P.char(':').surroundedBy(whitespaces0) *> recurse)

    val obj = rep0(kv).with1
      .between(P.char('{'), P.char('}'))
      .map(vs => Json.Obj(Chunk.fromIterable(vs)))

    P.oneOf(str :: num :: list :: obj :: bool :: pnull :: Nil)
  }

  // any whitespace followed by json followed by whitespace followed by end
  val parserFile: P[Json] = parser.between(whitespaces0, whitespaces0 ~ P.end)
}

object JsonStringUtil extends GenericStringUtil {
  // Here are the rules for escaping in json
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar),  // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )
}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable = decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

  val escapedToken: P[Unit] = {
    val escapes = P.charIn(decodeTable.keys.toSeq)

    val oct  = P.charIn('0' to '7')
    val octP = P.char('o') ~ oct ~ oct

    val hex  = P.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'))
    val hex2 = hex ~ hex
    val hexP = P.char('x') ~ hex2

    val hex4 = hex2 ~ hex2
    val u4   = P.char('u') ~ hex4
    val hex8 = hex4 ~ hex4
    val u8   = P.char('U') ~ hex8

    val after = P.oneOf[Any](escapes :: octP :: hexP :: u4 :: u8 :: Nil)
    (P.char('\\') ~ after).void
  }

  /** String content without the delimiter
    */
  def undelimitedString(endP: P[Unit]): P[String] =
    escapedToken.backtrack
      .orElse((!endP).with1 ~ P.anyChar)
      .rep
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_)     => P.fail
        }
      }

  private val simpleString: P0[String] =
    P.charsWhile0(c => c >= ' ' && c != '"' && c != '\\')

  def escapedString(q: Char): P[String] = {
    val end: P[Unit] = P.char(q)
    end *> ((simpleString <* end).backtrack
      .orElse(undelimitedString(end) <* end))
  }

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape = if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None      =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb                  = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt  =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~(str.length)
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None    =>
                c match {
                  case 'o'   => loop(decodeNum(idx + 2, 2, 8))
                  case 'x'   => loop(decodeNum(idx + 2, 2, 16))
                  case 'u'   => loop(decodeNum(idx + 2, 4, 16))
                  case 'U'   => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }
}
