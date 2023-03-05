package zio.parser

import scala.reflect.ClassTag

trait VersionSpecificPrinter[+Err, +Out, -Value] { self: Printer[Err, Out, Value] =>

  /** Prints this and if it fails, ignore the printed output and print 'that' instead. */
  final def orElseU[Err2 >: Err, Out2 >: Out, Value2: ClassTag](
      that: => Printer[Err2, Out2, Value2]
  ): Printer[Err2, Out2, Value | Value2] =
    orElseEither(that).contramap {
      case v2: Value2 => Right(v2)
      case v: Value   => Left(v)
    }

}
