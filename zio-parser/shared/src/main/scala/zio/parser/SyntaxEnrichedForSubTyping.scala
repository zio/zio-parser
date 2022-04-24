package zio.parser

import scala.reflect.ClassTag
import scala.language.implicitConversions

trait SyntaxEnrichedForSubTyping[A, I, O] {
  type Out <: A
  val tag: ClassTag[Out]
  val syntax: Syntax[String, I, O, Out, Out]
  val ev: Out <:< A
}

object SyntaxEnrichedForSubTyping {
  implicit def make[A, B <: A, I, O](
      s: Syntax[String, I, O, B, B]
  )(implicit ct: ClassTag[B]): SyntaxEnrichedForSubTyping[A, I, O] =
    new SyntaxEnrichedForSubTyping[A, I, O] {
      type Out = B
      val tag: ClassTag[B]                   = ct
      val syntax: Syntax[String, I, O, B, B] = s
      val ev: B <:< A                        = implicitly
    }
}
