package zio.parser

import scala.reflect.ClassTag

/** Members of [[Printer]] that exist only on some Scala versions. On Scala 3 it adds `orElseU`; on Scala 2 it has no
  * members.
  */
trait VersionSpecificPrinter[+Err, +Out, -Value] { self: Printer[Err, Out, Value] =>

  /** Combines this printer with 'that' into a printer of the union type. The side is chosen by the runtime class of the
    * value: a value of class `Value2` is printed with 'that', any other value with this printer. Unlike `orElse`, there
    * is no fallback to 'that' when this printer fails.
    */
  final def orElseU[Err2 >: Err, Out2 >: Out, Value1 <: Value, Value2: ClassTag](
      that: => Printer[Err2, Out2, Value2]
  )(implicit vtag: ClassTag[Value1]): Printer[Err2, Out2, Value1 | Value2] =
    orElseEither(that).contramap {
      case v2: Value2 => Right(v2)
      case v: Value1  => Left(v)
    }

}
