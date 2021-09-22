package zio.parser.benchmarks.lucene

import cats.parse.{Numbers, Parser0, Parser => P}
import zio.parser.benchmarks.lucene.Query.TopLevelCombinatorQuery

class CatsLuceneQueryParser(
    topLevelCombinator: List[Query] => TopLevelCombinatorQuery = Query.Or,
    esCompatible: Boolean = false
) {
  import CatsLuceneQueryParser._

  val numChar: P[Char] = Numbers.digit

  val escapedChar: P[String]      = (P.char('\\') *> P.anyChar).map(ch => "\\" + ch)
  val termStartChar: P[String]    =
    P.charWhere(ch => !invalidStartChars.contains(ch)).map(_.toString) | escapedChar
  val termChar: P[String]         = termStartChar | P.charIn('-', '+').map(_.toString)
  val whitespace: P[Unit]         = P.charIn(whitespaceChars).void
  val whitespaces: Parser0[Unit]  = whitespace.rep0.void
  val whitespaces1: Parser0[Unit] = whitespace.rep.void
  val quotedChar: P[String]       =
    P.charWhere(ch => ch != '\"' && ch != '\\').map(_.toString) | escapedChar

  val and: P[Unit]             =
    (P.string("AND").between(whitespaces, whitespaces1 | P.peek(P.char('('))) | P
      .string("&&")
      .surroundedBy(whitespaces)).backtrack.void
  val or: P[Unit]              =
    (P.string("OR").between(whitespaces, whitespaces1 | P.peek(P.char('('))) | P
      .string("||")
      .surroundedBy(whitespaces)).backtrack.void
  val not: P[Unit]             =
    (P.string("NOT").between(whitespaces, whitespaces1 | P.peek(P.char('('))) | P
      .char('!')
      .surroundedBy(whitespaces)).backtrack.void
  val plus: P[Unit]            = P.char('+').surroundedBy(whitespaces)
  val minus: P[Unit]           = P.char('-').surroundedBy(whitespaces)
  val lparen: P[Unit]          = P.char('(').surroundedBy(whitespaces)
  val rparen: P[Unit]          = P.char(')').surroundedBy(whitespaces)
  val opColon: P[Unit]         = P.char(':').surroundedBy(whitespaces)
  val opEqual: P[Unit]         = P.char('=').surroundedBy(whitespaces)
  val opLessThan: P[RangeOp]   = P.char('<').surroundedBy(whitespaces).as(RangeOp.LessThan)
  val opLessThanEq: P[RangeOp] =
    P.string("<=").surroundedBy(whitespaces).as(RangeOp.LessThanEq)
  val opMoreThan: P[RangeOp]   = P.char('>').surroundedBy(whitespaces).as(RangeOp.MoreThan)
  val opMoreThanEq: P[RangeOp] =
    P.string(">=").surroundedBy(whitespaces).as(RangeOp.MoreThanEq)
  val carat: P[Unit]           = P.char('^').surroundedBy(whitespaces)
  val tilde: P[Unit]           = P.char('~').surroundedBy(whitespaces)

  val quoted: P[String] = P.char('\"') *> quotedChar.rep0.string <* P.char('\"')

  val integer: P[Int]   = numChar.rep.string.map(_.toInt)
  val number: P[Double] = (numChar.rep ~ (P.char('.') *> numChar.rep).?).string.map(_.toDouble)

  val term: P[String] = (termStartChar ~ termChar.rep0)
    .map { case (head, tail) =>
      (head :: tail).mkString
    }
    .filter(s => !keywords.contains(s))
    .map(unescape)

  val regexpTerm: P[String] =
    P.char('/') *> (P.string("\\/").as('/') | P.charWhere(_ != '/')).rep0.string <* P.char('/')

  val rangeInStart: P[BoundType] =
    P.char('[').surroundedBy(whitespaces).as(BoundType.Inclusive)
  val rangeExStart: P[BoundType] =
    P.char('{').surroundedBy(whitespaces).as(BoundType.Exclusive)
  val rangeInEnd: P[BoundType]   = P.char(']').surroundedBy(whitespaces).as(BoundType.Inclusive)
  val rangeExEnd: P[BoundType]   = P.char('}').surroundedBy(whitespaces).as(BoundType.Exclusive)

  val rangeStart: P[BoundType] = rangeInStart | rangeExStart
  val rangeEnd: P[BoundType]   = rangeInEnd | rangeExEnd
  val rangeTo: P[Unit]         = P.string("TO").surroundedBy(whitespaces)

  val bound: P[String] =
    P.char('\"') *> quotedChar.rep0.string <* P.char('\"') |
      P.charWhere(ch => ch != ' ' && ch != ']' && ch != '}').rep.string

  val range: P[Query.Range] =
    for {
      lower <- (rangeStart ~ bound).map { case (t, s) => toBoundary(s, t) }
      _     <- rangeTo
      upper <- (bound ~ rangeEnd).map { case (s, t) => toBoundary(s, t) }
    } yield Query.Range(lower, upper)

  val modifier: P[Query => Query] =
    plus.as(Query.Require).backtrack | minus.as(Query.Prohibit).backtrack | not.as(Query.Not)

  val fieldName: P[String] = term.map(unescapeWildcards)

  val luceneFieldRange: P[Query.Range] =
    ((opLessThanEq.backtrack | opMoreThanEq.backtrack | opLessThan | opMoreThan) ~
      (quoted.backtrack | number.backtrack.string | term.backtrack)).map { case (op, term) =>
      op match {
        case RangeOp.LessThan   =>
          Query.Range(Boundary.Unbound, Boundary.Bound(term, BoundType.Exclusive))
        case RangeOp.LessThanEq =>
          Query.Range(Boundary.Unbound, Boundary.Bound(term, BoundType.Inclusive))
        case RangeOp.MoreThan   =>
          Query.Range(Boundary.Bound(term, BoundType.Exclusive), Boundary.Unbound)
        case RangeOp.MoreThanEq =>
          Query.Range(Boundary.Bound(term, BoundType.Inclusive), Boundary.Unbound)
      }
    }

  private def mergeRanges(r1: Query.Range, r2: Query.Range): Parser0[Query.Range] = {
    val lowers = (r1.lower, r2.lower)
    val uppers = (r2.lower, r2.upper)

    for {
      lower <- lowers match {
                 case (Boundary.Unbound, b: Boundary.Bound)        => P.pure(b)
                 case (b: Boundary.Bound, Boundary.Unbound)        => P.pure(b)
                 case (Boundary.Unbound, Boundary.Unbound)         => P.pure(Boundary.Unbound)
                 case (Boundary.Bound(_, _), Boundary.Bound(_, _)) =>
                   P.failWith("Two lower bounds provided with AND")
               }
      upper <- uppers match {
                 case (Boundary.Unbound, b: Boundary.Bound)        => P.pure(b)
                 case (b: Boundary.Bound, Boundary.Unbound)        => P.pure(b)
                 case (Boundary.Unbound, Boundary.Unbound)         => P.pure(Boundary.Unbound)
                 case (Boundary.Bound(_, _), Boundary.Bound(_, _)) =>
                   P.failWith("Two upper bounds provided with AND")
               }
    } yield Query.Range(lower, upper)
  }

  val esFieldRange: P[Query.Range] =
    luceneFieldRange.backtrack |
      ((luceneFieldRange <* and) ~ luceneFieldRange).between(lparen, rparen).backtrack.flatMap { case (r1, r2) =>
        mergeRanges(r1, r2)
      } |
      ((plus *> luceneFieldRange <* whitespaces) ~ (plus *> luceneFieldRange))
        .between(lparen, rparen)
        .backtrack
        .flatMap { case (r1, r2) =>
          mergeRanges(r1, r2)
        }

  val fieldRange: P[Query] =
    for {
      fieldName <- if (esCompatible) (fieldName <* opColon) else fieldName
      range     <- if (esCompatible) esFieldRange else luceneFieldRange
    } yield Query.Field(fieldName, range)

  val regexpTermQuery: P[Query] =
    regexpTerm.map(Query.Regex)

  val fuzzyOp: P[Int]     = tilde *> integer.?.map(_.getOrElse(2))
  val proximityOp: P[Int] = tilde *> integer

  val fieldTermQuery: P[Query] =
    ((term | number.map(_.toString)) ~ fuzzyOp.backtrack.?).map { case (term, maxEditDistance) =>
      Query.Term(term, maxEditDistance)
    }

  val quotedTermQuery: P[Query] =
    (quoted ~ proximityOp.backtrack.?).map { case (term, maxDistance) =>
      Query.Phrase(term, maxDistance)
    }

  val termQueryInner: P[Query] =
    regexpTermQuery | range | quotedTermQuery | fieldTermQuery

  val termQuery: P[Query] =
    (termQueryInner ~ (carat *> number).backtrack.?).map {
      case (query, Some(score)) => Query.Boost(query, score)
      case (query, None)        => query
    }

  val fieldNamePrefix: P[String] = fieldName <* (opColon | opEqual).surroundedBy(whitespaces)

  val query: P[Query] =
    P.recursive[Query] { recurse =>
      val grouping: P[Query] =
        (recurse.between(lparen, rparen) ~ (carat *> number).backtrack.?).map {
          case (query, Some(score)) => Query.Boost(query, score)
          case (query, None)        => query
        }

      val clause: P[Query] =
        fieldRange.backtrack |
          (fieldNamePrefix.backtrack.?.with1 ~ (grouping | termQuery))
            .map {
              case (Some(fieldName), query) => Query.Field(fieldName, query).normalize
              case (None, query)            => query
            }
            .surroundedBy(whitespaces)

      val modClause: P[Query] =
        (modifier.?.with1 ~ clause).map {
          case (Some(modFn), q) => modFn(q)
          case (None, q)        => q
        }

      val conjunctQuery: P[Query] =
        modClause.repSep(and).map { lst =>
          Query.And(lst.toList).normalize
        }

      val disjunctQuery: P[Query] =
        conjunctQuery.repSep(or).map { lst =>
          Query.Or(lst.toList).normalize
        }

      disjunctQuery.repSep(whitespaces).map { lst =>
        topLevelCombinator(lst.toList).normalize
      }
    }
}

object CatsLuceneQueryParser {
  val invalidStartChars: Set[Char] = Set(' ', '\t', '\n', '\r', '\u3000', '+', '-', '!', '(', ')', ':', '^', '<', '>',
    '=', '[', ']', '\"', '{', '}', '~', '\\', '/')

  val whitespaceChars: Set[Char] = Set(' ', '\t', '\n', '\r', '\u3000')

  val keywords: Set[String] = Set("AND", "OR", "NOT")

  sealed trait RangeOp
  object RangeOp {
    final object LessThan   extends RangeOp
    final object LessThanEq extends RangeOp
    final object MoreThan   extends RangeOp
    final object MoreThanEq extends RangeOp
  }

  private def unescape(s: String): String =
    s.replaceAll("""\\([^?*])""", "$1")

  private def unescapeWildcards(s: String): String =
    s.replaceAll("""\\([?*])""", "$1")

  private def toBoundary(term: String, boundType: BoundType): Boundary =
    if (term == "*") Boundary.Unbound else Boundary.Bound(term, boundType)
}
