package zio.parser

/** Members of [[Syntax]] that exist only on some Scala versions. On Scala 3 it adds `orElseU`; on Scala 2 it has no
  * members.
  */
trait VersionSpecificSyntax[+Err, -In, +Out, Value] {
  self: Syntax[Err, In, Out, Value] =>
}
