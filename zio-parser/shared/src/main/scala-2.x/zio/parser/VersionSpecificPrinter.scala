package zio.parser

trait VersionSpecificPrinter[+Err, +Out, -Value] {
  self: Printer[Err, Out, Value] =>
}
