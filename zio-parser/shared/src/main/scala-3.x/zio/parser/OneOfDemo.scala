package zio.parser

import scala.quoted._
import zio.parser.OneOfMain.Animal

object OneOfMain extends App {

  sealed trait Animal
  case object Dog extends Animal
  case object Cat extends Animal

  type Fish = Dog.type
  type FishSyntax = Syntax[String, Char, Char, Fish]
  val dogSyntax: FishSyntax =
    Syntax.string("dog", Dog)

  val catSyntax: Syntax[String, Char, Char, Cat.type] =
    Syntax.string("cat", Cat)


  inline def choice[A](
    x: Syntax[String, Char, Char, _ <: A],
    xs: Syntax[String, Char, Char, _ <: A]*): Syntax[String, Char, Char, A] = 
    ???

  val animalSyntax: Syntax[String, Char, Char, Animal] = 
    Macros.oneOf(dogSyntax, catSyntax)



  println("HELLO")
  println(animalSyntax.print(Dog))
  println(animalSyntax.print(Cat))
  println(animalSyntax.parseString("dog"))
  println(animalSyntax.parseString("cat"))
}
