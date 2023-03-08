package zio.parser

import scala.quoted._
import scala.reflect.ClassTag

object Macros {
  def oneOfImpl[A: Type, I: Type, O: Type](
      syntax: Expr[Syntax[String, I, O, _ <: A]],
      syntaxes: Expr[Seq[Syntax[String, I, O, _ <: A]]]
  )(using Quotes) = {
    import quotes.reflect._
    syntaxes match {
      case Varargs(exprs) =>
        val widenedSeq = Expr.ofSeq(exprs.prepended(syntax).map(widenImpl(_)))
        '{ $widenedSeq.reduce(_ | _) }
    }
  }

  private def widenImpl[A: Type, I: Type, O: Type](syntax: Expr[Syntax[String, I, O, _ <: A]])(using Quotes) = {
    import quotes.reflect._
    syntax.asExprOf[Any] match {
      case '{ $syntax: Syntax[String, I, O, a] } =>
        val classTag: Expr[ClassTag[a]] = summonClassTag[a]
        val ev: Expr[a <:< A]           = Expr.summon[a <:< A].get
        '{ $syntax.widen[A]($ev, $classTag) }
      case other                                 =>
        throw new Error(s"Unexpected syntax: $other")
    }
  }

  private def summonClassTag[A: Type](using Quotes): Expr[ClassTag[A]] = {
    import quotes.reflect._
    Expr.summon[ClassTag[A]] match {
      case Some(ct) => ct
      case None     => report.errorAndAbort(s"Could not summon ClassTag[${Type.show[A]}]")
    }
  }
}
