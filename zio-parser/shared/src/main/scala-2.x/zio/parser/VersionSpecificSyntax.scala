package zio.parser

trait VersionSpecificSyntax[+Err, -In, +Out, Value] {
  self: Syntax[Err, In, Out, Value] =>
}
