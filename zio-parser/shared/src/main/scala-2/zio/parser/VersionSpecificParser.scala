package zio.parser

/** Methods of [[Parser]] that exist only on some Scala versions: on Scala 3 it adds `orElseU`, on Scala 2 it adds
  * nothing.
  */
trait VersionSpecificParser[+Err, -In, +Result] {
  self: Parser[Err, In, Result] =>
}
