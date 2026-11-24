package zio.parser

import scala.deriving._

/** Converts between a case class `A` and `B`, the tuple of its fields (the field's own type for a case class with one
  * field). The `to` method of a parser and the `of` method of a syntax use it to turn a tuple result into a case class.
  * On Scala 2 a macro derives it; on Scala 3 it is derived from the case class's `Mirror`.
  */
trait TupleConversion[A, B] {

  /** Converts the case class value to its fields. */
  def to(a: A): B

  /** Builds the case class value from its fields. */
  def from(b: B): A
}

/** A [[TupleConversion]] built from the two conversion functions. */
class TupleConversionImpl[A, B](_to: A => B, _from: B => A) extends TupleConversion[A, B] {

  /** Converts the case class value to its fields. */
  def to(a: A): B = _to(a)

  /** Builds the case class value from its fields. */
  def from(b: B): A = _from(b)
}

/** Companion of [[TupleConversion]]. On Scala 2, `TupleConversion[P, T]` derives an instance with a macro; on Scala 3
  * the companion provides the given instances of [[ImplicitTupleConversion]].
  */
object TupleConversion extends ImplicitTupleConversion

/** Provides the implicit [[TupleConversion]] instances. The `zio.parser` package object extends it, so importing
  * `zio.parser._` brings them into scope.
  */
trait ImplicitTupleConversion {

  /** Derives the [[TupleConversion]] of a case class. On Scala 2 it is a macro, the same as `TupleConversion.apply`. On
    * Scala 3 it uses the case class's `Mirror` and converts to the tuple of all fields: `Tuple1` for one field and
    * `EmptyTuple` for none.
    */
  inline given autoTupleConversion[Prod <: Product](using
      m: Mirror.ProductOf[Prod]
  ): TupleConversion[Prod, m.MirroredElemTypes] =
    TupleConversionImpl[Prod, m.MirroredElemTypes](Tuple.fromProductTyped(_), m.fromProduct(_))

  /** On Scala 3, turns a conversion to `Tuple1[A]` into a conversion to `A`, so a case class with one field converts to
    * that field. On Scala 2 it exists only so that cross-built code can import it by name; calling it throws a
    * `scala.NotImplementedError`.
    */
  inline given autoTupleConversion1[Prod <: Product, A](using
      c: TupleConversion[Prod, Tuple1[A]]
  ): TupleConversion[Prod, A] =
    TupleConversionImpl[Prod, A](
      a =>
        val Tuple1(v) = c.to(a)
        v
      ,
      b => c.from(Tuple1(b))
    )
}
