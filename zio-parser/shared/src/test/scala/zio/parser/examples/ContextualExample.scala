package zio.parser.examples

import zio.parser.Parser.ParserError
import zio.parser.{Parser, Printer, Syntax, _}
import zio.test.Assertion._
import zio.test._

object ContextualExample extends ZIOSpecDefault {

  // Context sensitive example

  // Model
  case class Node(name: String, child: Option[Node])

  // Invertible syntax fragments
  val openTag: Syntax[String, Char, Char, String, String] =
    (Syntax.char('<') ~> Syntax.anyChar
      .filter[String, Char](_.isLetterOrDigit, "not a letter/digit")
      .repeat
      .string <~ Syntax.char('>')) ?? "open tag"

  def closeTag(name: String): Syntax[String, Char, Char, Unit, Unit] =
    Syntax.string(s"</$name>", ()) ?? s"close tag ($name)"

  // Separate definition of recursive parser
  lazy val nodeParser: Parser[String, Char, Node] =
    for {
      name  <- openTag.asParser
      inner <- nodeParser.optional
      _     <- closeTag(name).asParser
    } yield Node(name, inner)

  // Separate definition of recursive printer
  lazy val nodePrinter: Printer[String, Char, Node] = Printer.byValue { (model: Node) =>
    (model.child match {
      case Some(child) => openTag.asPrinter(model.name) ~> nodePrinter(child) ~> closeTag(model.name).asPrinter(())
      case None        => openTag.asPrinter(model.name) ~> closeTag(model.name).asPrinter(())
    })
  }

  // Plugging the two together to get an invertible syntax
  lazy val node: Syntax[String, Char, Char, Node, Node] =
    nodeParser <=> nodePrinter

  override def spec: ZSpec[Environment, Any] =
    suite("Contextual example")(
      suite("Separate parser")(
        test("simple") {
          assert(nodeParser.parseString("<hello></hello>"))(
            isRight(equalTo(Node("hello", None)))
          )
        },
        test("nested") {
          assert(nodeParser.parseString("<hello><world></world></hello>"))(
            isRight(equalTo(Node("hello", Some(Node("world", None)))))
          )
        }
      ),
      suite("Separate printer")(
        test("nested") {
          assert(nodePrinter.printString(Node("hello", Some(Node("world", None)))))(
            isRight(equalTo("<hello><world></world></hello>"))
          )
        }
      ),
      suite("Fused")(
        test("parse simple") {
          assert(node.parseString("<hello></hello>"))(
            isRight(equalTo(Node("hello", None)))
          )
        },
        test("parse nested") {
          assert(node.parseString("<hello><world></world></hello>"))(
            isRight(equalTo(Node("hello", Some(Node("world", None)))))
          )
        },
        test("parse wrong") {
          assert(node.parseString("<hello></world>"))(
            isLeft(equalTo(ParserError.Failure(List("close tag (hello)"), 7, "Not '</hello>'")))
          )
        },
        test("print simple") {
          assert(node.printString(Node("hello", None)))(
            isRight(equalTo("<hello></hello>"))
          )
        },
        test("print nested") {
          assert(node.printString(Node("hello", Some(Node("world", None)))))(
            isRight(equalTo("<hello><world></world></hello>"))
          )
        }
      )
    )
}
