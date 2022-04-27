package zio.parser

import scala.quoted._
import scala.reflect.ClassTag

object Macros {

  inline def oneOf[A](
    inline syntax: Syntax[String, Char, Char, _ <: A],
    inline syntaxes: Syntax[String, Char, Char, _ <: A]*,
  ): Syntax[String, Char, Char, A] =
    ${ Macros.oneOfImpl[A]('syntax, 'syntaxes) }

  def oneOfImpl[A: Type](
      syntax: Expr[Syntax[String, Char, Char, _ <: A]],
      syntaxes: Expr[Seq[Syntax[String, Char, Char, _ <: A]]],
    )(using Quotes) = {
      import quotes.reflect._

      syntaxes match {
        case Varargs(syntaxes) =>
          val widened = syntaxes.prepended(syntax).map(s => widenImpl(s))
          val widenedSeq = Expr.ofSeq(widened)
          '{ $widenedSeq.reduce(_ | _) }
      }
    }

  def widenImpl[A: Type](syntax: Expr[Syntax[String, Char, Char, _ <: A]])(using Quotes) = {
    import quotes.reflect._
    // This is weird huh
    syntax.asExprOf[Any] match {
      case '{ $syntax : Syntax[String, Char, Char, a] } =>
        val classTag: Expr[ClassTag[a]] = summonClassTag[a]
        val ev: Expr[a <:< A] = Expr.summon[a <:< A].get
        '{ $syntax.widen[A]($ev, $classTag) }
      case other => 
        throw new Error(s"Unexpected syntax: $other")
    }
  }

  private def summonClassTag[A: Type](using Quotes): Expr[ClassTag[A]] = {
    import quotes.reflect._
    Expr.summon[ClassTag[A]] match {
      case Some(ct) => ct
      case None => 
        report.errorAndAbort(s"Could not summon ClassTag[${Type.show[A]}]")
    }
  }

}