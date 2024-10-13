package zio.parser

import scala.deriving._

trait TupleConversion[A, B] {
  def to(a: A): B
  def from(b: B): A
}

class TupleConversionImpl[A, B](_to: A => B, _from: B => A) extends TupleConversion[A, B] {
  def to(a: A): B   = _to(a)
  def from(b: B): A = _from(b)
}

object TupleConversion extends ImplicitTupleConversion

trait ImplicitTupleConversion {
  inline given autoTupleConversion[Prod <: Product](using
      m: Mirror.ProductOf[Prod]
  ): TupleConversion[Prod, m.MirroredElemTypes] =
    TupleConversionImpl[Prod, m.MirroredElemTypes](Tuple.fromProductTyped(_), m.fromProduct(_))

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
