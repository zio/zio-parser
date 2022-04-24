package zio.parser.benchmarks.lucene

import zio.Chunk
import zio.parser._
import zio.parser.benchmarks.lucene.Query.TopLevelCombinatorQuery

class ZioLuceneQueryParser(
    topLevelCombinator: List[Query] => TopLevelCombinatorQuery = Query.Or,
    esCompatible: Boolean = false
) {
  import ZioLuceneQueryParser._

  val numChar       = Syntax.digit ?? "digit"
  val escapedChar   = (Syntax.char('\\') ~> Syntax.anyChar).transform(
    ch => "\\" + ch,
    (s: String) => s(1)
  ) ?? "escaped char"
  val termStartChar =
    (Syntax
      .charNotIn(invalidStartChars.toSeq: _*)
      .transform(_.toString, (s: String) => s(0)) | escapedChar) ?? "term start"
  val termChar      = (termStartChar | Syntax.charIn('-', '+').transform(_.toString, (s: String) => s(0))) ?? "term char"
  val whitespace    = Syntax.charIn(whitespaceChars.toSeq: _*).unit(' ') ?? "whitespace"
  val whitespaces   = whitespace.repeat0.asPrinted((), Chunk(())) ?? "any whitespace"
  val whitespaces1  = whitespace.repeat.asPrinted((), Chunk(())) ?? "at least 1 whitespace"
  val quotedChar    =
    (Syntax.charNotIn('\"', '\\').transform(_.toString, (s: String) => s(0)) | escapedChar) ?? "quoted char"

  val and =
    (Syntax.string("AND", ()).between(whitespaces, whitespaces1 | Syntax.char('(')) |
      Syntax.string("&&", ()).surroundedBy(whitespaces)).backtrack ?? "AND"
  val or  =
    (Syntax.string("OR", ()).between(whitespaces, whitespaces1 | Syntax.char('(')) |
      Syntax.string("||", ()).surroundedBy(whitespaces)).backtrack ?? "OR"
  val not =
    (Syntax.string("NOT", ()).between(whitespaces, whitespaces1 | Syntax.char('(')) |
      Syntax.char('!').surroundedBy(whitespaces)).backtrack ?? "NOT"

  val plus    = Syntax.char('+').surroundedBy(whitespaces) ?? "plus"
  val minus   = Syntax.char('-').surroundedBy(whitespaces) ?? "minus"
  val lparen  = Syntax.char('(').surroundedBy(whitespaces) ?? "lparen"
  val rparen  = Syntax.char(')').surroundedBy(whitespaces) ?? "rparen"
  val opColon = Syntax.char(':').surroundedBy(whitespaces) ?? "opColon"
  val opEqual = Syntax.char('=').surroundedBy(whitespaces) ?? "opEqual"
  val carat   = Syntax.char('^').surroundedBy(whitespaces) ?? "carat"
  val tilde   = Syntax.char('~').surroundedBy(whitespaces) ?? "tilde"

  val opLessThan   = Syntax.string("<", RangeOp.LessThan).surroundedBy(whitespaces)
  val opLessThanEq = Syntax.string("<=", RangeOp.LessThanEq).surroundedBy(whitespaces)
  val opMoreThan   = Syntax.string(">", RangeOp.MoreThan).surroundedBy(whitespaces)
  val opMoreThanEq = Syntax.string(">=", RangeOp.MoreThanEq).surroundedBy(whitespaces)

  val quoted  = quotedChar.repeat0.flatten.surroundedBy(Syntax.char('\"')) ?? "quoted"
  val integer = numChar.repeat.transform(
    chars => chars.mkString.toInt,
    (num: Int) => Chunk.fromArray(num.toString.toCharArray)
  ) ?? "integer"

  val number = (numChar.repeat ~ (Syntax.char('.') ~> numChar.repeat).?).string.transform(
    _.toDouble,
    (d: Double) => d.toString
  ) ?? "number"

  val term = (termStartChar ~ termChar.repeat0)
    .transform(
      { case (head, tail) => unescape(head + tail.mkString) },
      (s: String) => (s.head.toString, Chunk(s.tail)) // TODO: escape
    )
    .filter((s: String) => !keywords.contains(s), "reserved keyword") ?? "term"

  val regexpTerm =
    (Syntax.string("\\/", '/') | Syntax.charNotIn('/')).repeat0.string.surroundedBy(Syntax.char('/')) ?? "regexpTerm"

  val rangeInStart = Syntax.string("[", BoundType.Inclusive).surroundedBy(whitespaces) ?? "rangeInStart"
  val rangeExStart = Syntax.string("{", BoundType.Exclusive).surroundedBy(whitespaces) ?? "rangeExStart"
  val rangeInEnd   = Syntax.string("]", BoundType.Inclusive).surroundedBy(whitespaces) ?? "rangeInEnd"
  val rangeExEnd   = Syntax.string("}", BoundType.Exclusive).surroundedBy(whitespaces) ?? "rangeExEnd"

  val rangeStart = (rangeInStart.widen[BoundType] | rangeExStart.widen[BoundType]) ?? "rangeStart"
  val rangeEnd   = (rangeInEnd.widen[BoundType] | rangeExEnd.widen[BoundType]) ?? "rangeEnd"
  val rangeTo    = (Syntax.string("TO", ()).surroundedBy(whitespaces)) ?? "rangeTo"

  val bound =
    (quotedChar.repeat0.flatten
      .surroundedBy(Syntax.char('\"')) | Syntax.charNotIn(' ', ']', '}').repeat.string) ?? "bound"

  val range =
    (
      ((rangeStart ~ bound).transform[Boundary](
        { case (t, s) => toBoundary(s, t) },
        {
          case Boundary.Bound(value, boundType) => (boundType, value)
          case Boundary.Unbound                 => (BoundType.Exclusive, "*")
        }
      ) ~ rangeTo ~
        (bound ~ rangeEnd).transform[Boundary](
          { case (s, t) => toBoundary(s, t) },
          {
            case Boundary.Bound(value, boundType) => (value, boundType)
            case Boundary.Unbound                 => ("*", BoundType.Exclusive)
          }
        )).transform(
        { case (lower, upper) => Query.Range(lower, upper) },
        (r: Query.Range) => (r.lower, r.upper)
      )
    ) ?? "range"

  sealed trait Mod
  object Mod {
    case object Require  extends Mod
    case object Prohibit extends Mod
    case object Not      extends Mod
  }

  val modifier =
    (plus.as(Mod.Require).backtrack.widen[Mod] |
      minus.as(Mod.Prohibit).backtrack.widen[Mod] |
      not
        .as(Mod.Not)
        .widen[Mod]) ?? "modifier"

  val fieldName = term.transform(unescapeWildcards, identity[String]) ?? "fieldName" // TODO: escapeWildcards

  val luceneFieldRange =
    (
      ((opLessThanEq.backtrack.widen[RangeOp] | opMoreThanEq.backtrack.widen[RangeOp] | opLessThan
        .widen[RangeOp] | opMoreThan
        .widen[RangeOp]) ~
        (quoted.backtrack | number.backtrack.transform(_.toString, (s: String) => s.toDouble) | term.backtrack))
        .transform(
          { case (op, term) =>
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
          },
          (r: Query.Range) =>
            r.lower match {
              case Boundary.Bound(value, BoundType.Exclusive) =>
                (RangeOp.MoreThan, value)
              case Boundary.Bound(value, BoundType.Inclusive) =>
                (RangeOp.MoreThanEq, value)
              case Boundary.Unbound                           =>
                r.upper match {
                  case Boundary.Bound(value, BoundType.Exclusive) =>
                    (RangeOp.LessThan, value)
                  case Boundary.Bound(value, BoundType.Inclusive) =>
                    (RangeOp.LessThanEq, value)
                }
            }
        )
    ) ?? "luceneFieldRange"

  private def mergeRanges(r1: Query.Range, r2: Query.Range): Either[String, Query.Range] = {
    val lowers = (r1.lower, r2.lower)
    val uppers = (r2.lower, r2.upper)

    for {
      lower <- lowers match {
                 case (Boundary.Unbound, b: Boundary.Bound)        => Right(b)
                 case (b: Boundary.Bound, Boundary.Unbound)        => Right(b)
                 case (Boundary.Unbound, Boundary.Unbound)         => Right(Boundary.Unbound)
                 case (Boundary.Bound(_, _), Boundary.Bound(_, _)) =>
                   Left("Two lower bounds provided with AND")
               }
      upper <- uppers match {
                 case (Boundary.Unbound, b: Boundary.Bound)        => Right(b)
                 case (b: Boundary.Bound, Boundary.Unbound)        => Right(b)
                 case (Boundary.Unbound, Boundary.Unbound)         => Right(Boundary.Unbound)
                 case (Boundary.Bound(_, _), Boundary.Bound(_, _)) =>
                   Left("Two upper bounds provided with AND")
               }
    } yield Query.Range(lower, upper)
  }

  private def splitRanges(r: Query.Range): Either[String, (Query.Range, Query.Range)] =
    ??? // TODO

  val esFieldRange =
    (luceneFieldRange.backtrack |
      ((luceneFieldRange <~ and) ~ luceneFieldRange)
        .between(lparen, rparen)
        .backtrack
        .transformEither((mergeRanges _).tupled, splitRanges) |
      ((plus ~> luceneFieldRange <~ whitespaces) ~ (plus ~> luceneFieldRange))
        .between(lparen, rparen)
        .backtrack
        .transformEither((mergeRanges _).tupled, splitRanges)) ?? "esFieldRange"

  val fieldRange =
    ((if (esCompatible) (fieldName <~ opColon) else fieldName) ~
      (if (esCompatible) esFieldRange else luceneFieldRange)).transform(
      { case (fieldName, range) => Query.Field(fieldName, range) },
      (f: Query.Field) => (f.fieldName, f.query.asInstanceOf[Query.Range]) // TODO: transformEither instead of cast
    ) ?? "fieldRange"

  val regexpTermQuery = regexpTerm.transform(Query.Regex, (r: Query.Regex) => r.value) ?? "regexpTermQuery"

  val fuzzyOp     = tilde ~> integer.?.transform[Int](
    _.getOrElse(2),
    {
      case 2      => None
      case n: Int => Some(n)
    }
  ) ?? "fuzzyOp"
  val proximityOp = (tilde ~> integer) ?? "proximityOp"

  val fieldTermQuery =
    ((term | number.transform(_.toString, (s: String) => s.toDouble)) ~ fuzzyOp.backtrack.?).transform(
      { case (term, maxEditDistance) => Query.Term(term, maxEditDistance) },
      (t: Query.Term) => (t.value, t.maxEditDistances)
    ) ?? "fieldTermQuery"

  val quotedTermQuery =
    (quoted ~ proximityOp.backtrack.?).transform(
      { case (term, maxDistance) => Query.Phrase(term, maxDistance) },
      (p: Query.Phrase) => (p.value, p.maxDistance)
    ) ?? "quotedTermQuery"

  val termQueryInner =
    (regexpTermQuery.widen[Query] | range.widen[Query] | quotedTermQuery.widen[Query] | fieldTermQuery
      .widen[Query]) ?? "termQueryInner"

  val termQuery = (termQueryInner ~ (carat ~> number).backtrack.?).transform[Query](
    {
      case (query, Some(score)) => Query.Boost(query, score)
      case (query, None)        => query
    },
    {
      case Query.Boost(inner, score) => (inner, Some(score))
      case other: Query              => (other, None)
    }
  ) ?? "termQuery"

  val fieldNamePrefix = (fieldName <~ (opColon | opEqual).surroundedBy(whitespaces)) ?? "fieldNamePrefix"

  lazy val grouping: Syntax[String, Char, Char, Query] =
    (query.between(lparen, rparen) ~ (carat ~> number).backtrack.?).transform[Query](
      {
        case (query, Some(score)) => Query.Boost(query, score)
        case (query, None)        => query
      },
      {
        case Query.Boost(inner, score) => (inner, Some(score))
        case other: Query              => (other, None)
      }
    ) ?? "grouping"

  lazy val clause: Syntax[String, Char, Char, Query] = fieldRange.backtrack.widen[Query] |
    (fieldNamePrefix.backtrack.? ~ (grouping | termQuery))
      .transform[Query](
        {
          case (Some(fieldName), query) => Query.Field(fieldName, query).normalize
          case (None, query)            => query
        },
        {
          case Query.Field(fieldName, query) => (Some(fieldName), query)
          case other: Query                  => (None, other)
        }
      )
      .surroundedBy(whitespaces) ?? "clause"

  val modClause = (modifier.? ~ clause).transform[Query](
    {
      case (Some(Mod.Require), query)  => Query.Require(query)
      case (Some(Mod.Prohibit), query) => Query.Prohibit(query)
      case (Some(Mod.Not), query)      => Query.Not(query)
      case (None, query)               => query
    },
    {
      case Query.Require(query)  => (Some(Mod.Require), query)
      case Query.Prohibit(query) => (Some(Mod.Prohibit), query)
      case Query.Not(query)      => (Some(Mod.Not), query)
      case other: Query          => (None, other)
    }
  ) ?? "modClause"

  lazy val conjunctQuery = modClause
    .repeatWithSep(and)
    .transform[Query](
      chunk => Query.And(chunk.toList).normalize,
      { case (and: Query.And) => Chunk.fromIterable(and.queries) }
    )
    .widen[Query] ?? "conjunctQuery"

  lazy val disjunctQuery = conjunctQuery
    .repeatWithSep(or)
    .transform[Query](
      chunk => Query.Or(chunk.toList).normalize,
      { case (or: Query.Or) => Chunk.fromIterable(or.queries) }
    )
    .widen[Query] ?? "disjunctQuery"

  lazy val query: Syntax[String, Char, Char, Query] =
    disjunctQuery
      .repeatWithSep(whitespaces)
      .transform[Query](
        chunk => topLevelCombinator(chunk.toList).normalize,
        { case Query.Or(lst) =>
          Chunk.fromIterable(lst) // TODO: other cases
        }
      )
      .manualBacktracking ?? "query"
}

object ZioLuceneQueryParser {
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
