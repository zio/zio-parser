package zio.parser.presentation.lucene

sealed trait Query
object Query {
  sealed trait TopLevelCombinatorQuery                                extends Query                   {
    def normalize: Query
  }
  final case class Or(queries: List[Query])                           extends TopLevelCombinatorQuery {
    def normalize: Query =
      queries match {
        case Nil         => this
        case head :: Nil => head
        case _           =>
          Or(queries.map {
            case Require(inner)  => inner
            case Prohibit(inner) => Not(inner)
            case other: Query    => other
          })
      }
  }
  final case class And(queries: List[Query])                          extends TopLevelCombinatorQuery {
    def normalize: Query =
      queries match {
        case Nil         => this
        case head :: Nil => head
        case _           =>
          And(queries.map {
            case Require(inner)  => inner
            case Prohibit(inner) => Not(inner)
            case other: Query    => other
          })
      }
  }
  final case class Not(query: Query)                                  extends Query
  final case class Require(query: Query)                              extends Query
  final case class Prohibit(query: Query)                             extends Query
  final case class Boost(query: Query, score: Double)                 extends Query
  final case class Range(lower: Boundary, upper: Boundary)            extends Query
  final case class Regex(value: String)                               extends Query
  final case class Term(value: String, maxEditDistances: Option[Int]) extends Query
  final case class Phrase(value: String, maxDistance: Option[Int])    extends Query
  final case class Field(fieldName: String, query: Query)             extends Query                   {
    def normalize: Query =
      query match {
        case Boost(And(queries), boost) =>
          Boost(And(queries.map(Field(fieldName, _))), boost)
        case And(queries)               =>
          And(queries.map(Field(fieldName, _)))
        case Boost(Or(queries), boost)  =>
          Boost(Or(queries.map(Field(fieldName, _))), boost)
        case Or(queries)                =>
          Or(queries.map(Field(fieldName, _)))
        case _                          => this
      }
  }
}

sealed trait Boundary
object Boundary {
  final case class Bound(value: String, boundType: BoundType) extends Boundary
  case object Unbound                                         extends Boundary
}

sealed trait BoundType
object BoundType {
  case object Inclusive extends BoundType
  case object Exclusive extends BoundType
}
