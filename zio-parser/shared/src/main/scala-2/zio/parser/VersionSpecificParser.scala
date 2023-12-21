package zio.parser

trait VersionSpecificParser[+Err, -In, +Result] {
  self: Parser[Err, In, Result] =>
}
