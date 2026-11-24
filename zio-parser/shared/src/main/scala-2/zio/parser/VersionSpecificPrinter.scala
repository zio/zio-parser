package zio.parser

/** Members of [[Printer]] that exist only on some Scala versions. On Scala 3 it adds `orElseU`; on Scala 2 it has no
  * members.
  */
trait VersionSpecificPrinter[+Err, +Out, -Value] {
  self: Printer[Err, Out, Value] =>
}
