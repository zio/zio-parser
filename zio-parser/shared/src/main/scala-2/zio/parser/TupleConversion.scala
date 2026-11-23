package zio.parser

import scala.annotation.nowarn
import scala.reflect.macros.whitebox

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

/** Companion of [[TupleConversion]]. On Scala 2, `TupleConversion[P, T]` derives an instance with a macro; on Scala 3
  * the companion provides the given instances of [[ImplicitTupleConversion]].
  */
object TupleConversion {

  /** Derives the conversion between the case class `P` and `T`, the tuple of its fields (the field's own type for one
    * field). Compilation fails with "Type ... is not a case class" for other types, and with "Type ... has multiple
    * parameter lists which is currently not supported" for a case class with more than one parameter list.
    */
  def apply[P, T]: TupleConversion[P, T] = macro genTupleConversion[P, T]

  /** The macro implementation of `apply` and `autoTupleConversion`. Currently it also fails, with a compile error, for
    * a case class whose parameter list is empty (`case class Empty()`).
    */
  @nowarn def genTupleConversion[P: c.WeakTypeTag, T: c.WeakTypeTag](
      c: whitebox.Context
  ): c.Expr[TupleConversion[P, T]] = {
    import c.universe._
    val prodTpe    = c.weakTypeOf[P]
    if (
      !prodTpe.typeSymbol.isClass ||
      !prodTpe.typeSymbol.asClass.isCaseClass
    ) {
      c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} is not a case class")
    }
    val paramLists = prodTpe.typeSymbol.asClass.primaryConstructor.asMethod.typeSignatureIn(prodTpe).paramLists
    val result     = paramLists match {
      case List(List(singleParam)) =>
        // Special case: single parameter products

        val typ = singleParam.typeSignatureIn(prodTpe).finalResultType

        q"""new _root_.zio.parser.TupleConversion[$prodTpe, $typ] {
                override def to(a: $prodTpe): $typ = a.${singleParam.name.toTermName}                
                override def from(b: $typ): $prodTpe =
                    new ${prodTpe}(b)
        }
        """

      case List(params) =>
        // Generic case: n > 1 parameters

        val tupleName   = definitions.TupleClass(params.size).name.toTypeName
        val tupleParams = params.map { sym =>
          sym.typeSignatureIn(prodTpe).finalResultType
        }
        val tup         = tq"$tupleName[..$tupleParams]"
        val packers     =
          params.map { sym =>
            val symTerm = sym.name.toTermName
            q"a.$symTerm"
          }
        val unpackers   =
          params.indices.map { idx =>
            val accessor = TermName(s"_${idx + 1}")
            q"b.$accessor"
          }

        q"""new _root_.zio.parser.TupleConversion[$prodTpe, $tup] {
                override def to(a: $prodTpe): $tup =
                (..$packers)
                override def from(b: $tup): $prodTpe =
                new ${prodTpe}(..$unpackers)
            }
            """
      case Nil          =>
        // Special case: zero parameter products
        q"""new _root_.zio.parser.TupleConversion[$prodTpe, Unit] {
                override def to(a: $prodTpe): Unit = ()
                override def from(b: Unit): $prodTpe =
                    new $prodTpe()
            }
        """
      case _            =>
        c.abort(
          c.enclosingPosition,
          s"Type ${prodTpe.typeSymbol} has multiple parameter lists which is currently not supported"
        )
    }

    c.Expr(result)
  }
}

/** Provides the implicit [[TupleConversion]] instances. The `zio.parser` package object extends it, so importing
  * `zio.parser._` brings them into scope.
  */
trait ImplicitTupleConversion {

  /** Derives the [[TupleConversion]] of a case class. On Scala 2 it is a macro, the same as `TupleConversion.apply`. On
    * Scala 3 it uses the case class's `Mirror` and converts to the tuple of all fields: `Tuple1` for one field and
    * `EmptyTuple` for none.
    */
  implicit def autoTupleConversion[P <: Product, T]: TupleConversion[P, T] =
    macro TupleConversion.genTupleConversion[P, T]

  /** On Scala 3, turns a conversion to `Tuple1[A]` into a conversion to `A`, so a case class with one field converts to
    * that field. On Scala 2 it exists only so that cross-built code can import it by name; calling it throws a
    * `scala.NotImplementedError`.
    */
  def autoTupleConversion1[P, A](implicit ev: TupleConversion[P, Tuple1[A]]): TupleConversion[P, A] =
    ??? // defined here so it can be used in explicit imports when cross-compiling
}
