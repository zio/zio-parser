//package zio.parser.presentation
//
//import zio.parser.ParserErrorPrinter.ParserErrorOps
//import zio.parser.presentation.lucene.LuceneQueryParser
//import zio.parser.{Parser, Printer, Syntax}
//import zio.{Console, ZEnv, ZIO, ZIOAppArgs, ZIOAppDefault}
//
//import java.io.IOException
//
//object Presentation extends ZIOAppDefault {
//
//  /*
//   There are lot of different parser libraries in the Scala ecosystem. - show some popular ones
//
//   Let me introduce a new one that I was working on a little last year.
//
//   Unsurprisingly it's called zio-parser.
//
//   So why one more parser library?
//   zio-parser is an invertible parser combinator library
//
//  - wanted to see if the invertible parser idea can be implemented in a way that
//     - it's at least as convenient as other parser combinator libraries, while still generating printers too
//     - when this hits a its limits, ability to fall back to parser-only or printer-only
//     - usual features and integrations of the ZIO ecosystem - custom error types, usage of variance, chunk/zstream, etc.
//
//  - good error reporting
//  - good enough performance
//
//  Let's see a few examples of how the library feels like
//   */
//
//  // First example
//
////  object FirstExampleNoTypeSigs {
////    sealed trait OpType
////    object OpType {
////      case object Add extends OpType
////      case object Sub extends OpType
////    }
////
////    sealed trait Expr
////    object Expr {
////      final case class Const(value: Int)                      extends Expr
////      final case class Op(operator: OpType, a: Expr, b: Expr) extends Expr
////    }
////
////    val const =
////      Syntax.digit.repeat.string
////        .transformTo[String, Expr, Expr](
////          s => Expr.Const(s.toInt),
////          { case (n: Expr.Const) => n.value.toString },
////          "Not a constant"
////        ) ?? "constant"
////
////    val add = Syntax
////      .char('+')
////      .transformTo[String, OpType, OpType](_ => OpType.Add, { case OpType.Add => '+' }, "Not +")
////    val sub = Syntax
////      .char('-')
////      .transformTo[String, OpType, OpType](_ => OpType.Sub, { case OpType.Sub => '-' }, "Not -")
////
////    val operator =
////      (add <> sub) ?? "operator"
////
////    val lParen = Syntax.char('(')
////    val rParen = Syntax.char(')')
////
////    lazy val subExpr =
////      (expr ~ operator ~ expr)
////        .transformTo[String, Expr, Expr](
////          { case (a, op, b) => Expr.Op(op, a, b) },
////          { case (op: Expr.Op) => (op.a, op.operator, op.b) },
////          "Not valid sub expression"
////        ) ?? "sub expression"
////
////    lazy val subExprInParens =
////      lParen ~> subExpr <~ rParen
////
////    lazy val expr =
////      (subExprInParens | const) ?? "expression"
////  }
//
//  object FirstExample {
//    sealed trait OpType
//    object OpType {
//      case object Add extends OpType
//      case object Sub extends OpType
//    }
//
//    sealed trait Expr
//    object Expr {
//      final case class Const(value: Int)                      extends Expr
//      final case class Op(operator: OpType, a: Expr, b: Expr) extends Expr
//    }
//
//    val const: Syntax[String, Char, Char, Expr, Expr] =
//      Syntax.digit.repeat.string
//        .transformTo[String, Expr, Expr](
//          s => Expr.Const(s.toInt),
//          { case (n: Expr.Const) => n.value.toString },
//          "Not a constant"
//        ) ?? "constant"
//
//    val add = Syntax
//      .char('+')
//      .transformTo[String, OpType, OpType](_ => OpType.Add, { case OpType.Add => '+' }, "Not +")
//    val sub = Syntax
//      .char('-')
//      .transformTo[String, OpType, OpType](_ => OpType.Sub, { case OpType.Sub => '-' }, "Not -")
//
//    val operator: Syntax[String, Char, Char, OpType, OpType] =
//      (add <> sub) ?? "operator"
//
//    val lParen: Syntax[String, Char, Char, Unit, Unit] = Syntax.char('(')
//    val rParen: Syntax[String, Char, Char, Unit, Unit] = Syntax.char(')')
//
//    lazy val subExpr: Syntax[String, Char, Char, Expr, Expr] =
//      (expr ~ operator ~ expr)
//        .transformTo[String, Expr, Expr](
//          { case (a, op, b) => Expr.Op(op, a, b) },
//          { case (op: Expr.Op) => (op.a, op.operator, op.b) },
//          "Not valid sub expression"
//        ) ?? "sub expression"
//
//    lazy val subExprInParens: Syntax[String, Char, Char, Expr, Expr] =
//      lParen ~> subExpr <~ rParen
//
//    lazy val expr: Syntax[String, Char, Char, Expr, Expr] =
//      (subExprInParens | const) ?? "expression"
//
//    def demo(): ZIO[Console, IOException, Unit] = {
//      val parsed: Either[Parser.ParserError[String], Expr] =
//        expr.parseString("((((123+456)-789)+(0+(1+2)))-3)")
//
//      val input2: String                                    = "((((123+456)*789)+(0+(1+2)))-3)"
//      val parsed2: Either[Parser.ParserError[String], Expr] =
//        expr.parseString(input2)
//
//      val colorMap = Map(
//        "operator" -> scala.Console.YELLOW,
//        "constant" -> scala.Console.BLUE
//      )
//
//      for {
//        _ <- Console.printLine(parsed)
//        _ <- Console.printLine(expr.printString(parsed.toOption.get))
//        _ <- Console.printLine(expr.printString(parsed.toOption.get, colorMap))
//
//        _ <- Console.printLine(parsed2)
//        _ <- parsed2.left.toOption.get.prettyPrint(input2)
//      } yield ()
//    }
//  }
//
//  /*
//  First show without type signatures
//  Highlight that:
//    - usual parser combinator fragments like .string, .repeat, <>, ~, etc
//    - ?? to give descriptive names to parsers
//    - transformTo: instead of map - we need both directions
//
//   => show the types, fade in a big Syntax type sig:
//    class Syntax[+Err, -In, +Out, -Value, +Result] private (
//      val asParser: Parser[Err, In, Result],
//      val asPrinter: Printer[Err, Out, Value, Result]
//    )
//
//    => Show the demo inputs and outputs
//
//
//    Next:
//
//    This was a simple example, mention (show a codemap of) two real-world examples:
//      - lucene query parser
//      - caliban parser
//
//    show example input and output for both:
//   */
//
//  object SecondExample {
//    def demo(): ZIO[Console, IOException, Unit] = {
//
//      val luceneSyntax = new LuceneQueryParser().query
//
//      val luceneInput1  =
//        "status:(active OR pending) AND title:(full text search)^2 AND date:[2012-01-01 TO 2012-12-31] AND (quikc~ brwn~ foks~)"
//      val luceneResult1 = luceneSyntax.parseString(luceneInput1)
//
//      val luceneInput2  =
//        "status:(active OR pending) AND title:(full text search)^x AND date:[2012-01-01 TO 2012-12-31] AND (quikc~ brwn~ foks~)"
//      val luceneResult2 = (luceneSyntax <~ Syntax.end).parseString(luceneInput2)
//
//      val calibanInput1 =
//        """type Character {
//          |  name: String!
//          |  age: Int!
//          |}
//          |
//          |type Queries {
//          |  characters: [Character!]!
//          |  character(name: String!): Character
//          |}
//          |""".stripMargin
//      val calibanResult1 =
//        CalibanParser.document.parseString(calibanInput1)
//
//      val calibanInput2 =
//        """type Character {
//          |  name: String!
//          |  age: Int!
//          |}
//          |
//          |struct Queries {
//          |  characters: [Character!]!
//          |  character(name: String!): Character
//          |}
//          |""".stripMargin
//      val calibanResult2 =
//        CalibanParser.document.parseString(calibanInput2)
//
//      for {
////        _ <- Console.printLine(luceneResult1)
//        _ <- Console.printLine(luceneSyntax.printString(luceneResult1.toOption.get))
//        _ <- Console.printLine(.printString(luceneResult1.toOption.get))
////        _ <- Console.printLine(luceneResult2)
////        _ <- luceneResult2.left.toOption.get.prettyPrint(luceneInput2)
////
////        _ <- Console.printLine(calibanResult1)
////        _ <- Console.printLine(CalibanParser.document.printString(calibanResult1.toOption.get))
////        _ <- Console.printLine(calibanResult2)
////        _ <- calibanResult2.left.toOption.get.prettyPrint(calibanInput2)
//      } yield ()
//    }
//  }
//
//  object ThirdExample {
//    // Model
//    case class Node(name: String, child: Option[Node])
//
//    // Invertible syntax fragments
//    val openTag: Syntax[String, Char, Char, String, String] =
//      (Syntax.char('<') ~> Syntax.anyChar
//        .filter[String, Char](_.isLetterOrDigit, "not a letter/digit")
//        .repeat
//        .string <~ Syntax.char('>')) ?? "open tag"
//
//    def closeTag(name: String): Syntax[String, Char, Char, Unit, Unit] =
//      Syntax.string(s"</$name>", ()) ?? s"close tag ($name)"
//
//    // Separate definition of recursive parser
//    lazy val nodeParser: Parser[String, Char, Node] =
//      for {
//        name  <- openTag.asParser
//        inner <- nodeParser.optional
//        _     <- closeTag(name).asParser
//      } yield Node(name, inner)
//
//    // Separate definition of recursive printer
//    lazy val nodePrinter: Printer[String, Char, Node, Node] = Printer.byValue { (model: Node) =>
//      (model.child match {
//        case Some(child) => openTag.asPrinter(model.name) ~> nodePrinter(child) ~> closeTag(model.name).asPrinter(())
//        case None        => openTag.asPrinter(model.name) ~> closeTag(model.name).asPrinter(())
//      }).as(model)
//    }
//
//    // Plugging the two together to get an invertible syntax
//    lazy val node: Syntax[String, Char, Char, Node, Node] =
//      (nodeParser <=> nodePrinter) <~ Syntax.end
//
//    def demo(): ZIO[Console, IOException, Unit] = {
//      val input = "<hello><world></world></hello>"
//      val parsed = node.parseString(input)
//
//      val input2 = "<world><hello></world></hello>"
//      val parsed2 = node.parseString(input2)
//
//      for {
//        _ <- Console.printLine(parsed)
//        _ <- Console.printLine(node.printString(parsed.toOption.get))
//
////        _ <- Console.printLine(parsed2)
//        _ <- parsed2.left.toOption.get.prettyPrint(input2)
//      } yield ()
//    }
//  }
//
//  /*
//  Some other features:
//
//  Auto/manual backtracking
//  Pipelineing
//  Fusing
//   */
//
//  // printer-only features
//  // indentation
//  // syntax highlight based on name stack?
//
//  //  And now let's talk about what's the current status
//
//  // released version for JVM, JS, Native, using latest ZIO 2 RC
//  // the error pretty printing is not merged yet
//  // only string input
//  // advanced printer features like indentation and syntax highlight not in there
//
//  override def run: ZIO[ZEnv with ZIOAppArgs, Any, Any] =
//    for {
////      _ <- FirstExample.demo()
//      _ <- SecondExample.demo()
////      _ <- ThirdExample.demo()
//    } yield ()
//}
