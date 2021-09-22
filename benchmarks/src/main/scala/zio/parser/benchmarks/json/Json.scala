package zio.parser.benchmarks.json

import zio.Chunk

sealed abstract class Json
object Json {
  final case class Obj(fields: Chunk[(String, Json)]) extends Json
  final case class Arr(elements: Chunk[Json])         extends Json
  final case class Bool(value: Boolean)               extends Json
  final case class Str(value: String)                 extends Json
  final case class Num(value: BigDecimal)             extends Json
  case object Null                                    extends Json
}
