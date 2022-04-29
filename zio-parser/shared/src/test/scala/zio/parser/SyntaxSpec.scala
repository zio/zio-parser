package zio.parser

import zio.parser.Parser.ParserError.AllBranchesFailed
import zio.test.Assertion._
import zio.test._

object SyntaxSpec extends ZIOSpecDefault {

  sealed trait WeekDay {
    self =>
    override def toString: String =
      self match {
        case WeekDay.Monday    => "Monday"
        case WeekDay.Tuesday   => "Tuesday"
        case WeekDay.Wednesday => "Wednesday"
        case WeekDay.Thursday  => "Thursday"
        case WeekDay.Friday    => "Friday"
        case WeekDay.Saturday  => "Saturday"
        case WeekDay.Sunday    => "Sunday"
      }
  }

  object WeekDay {
    case object Monday extends WeekDay

    case object Tuesday extends WeekDay

    case object Wednesday extends WeekDay

    case object Thursday extends WeekDay

    case object Friday extends WeekDay

    case object Saturday extends WeekDay

    case object Sunday extends WeekDay

    val mondaySyntax    = Syntax.string("Mon", Monday)
    val tuesdaySyntax   = Syntax.string("Tue", Tuesday)
    val wednesdaySyntax = Syntax.string("Wed", Wednesday)
    val thursdaySyntax  = Syntax.string("Thu", Thursday)
    val fridaySyntax    = Syntax.string("Fri", Friday)
    val saturdaySyntax  = Syntax.string("Sat", Saturday)
    val sundaySyntax    = Syntax.string("Sun", Sunday)

    val arbitrary: Gen[Any, WeekDay] =
      Gen.oneOf(
        Gen.const(Monday),
        Gen.const(Tuesday),
        Gen.const(Wednesday),
        Gen.const(Thursday),
        Gen.const(Friday),
        Gen.const(Saturday),
        Gen.const(Sunday)
      )

    val weekDaySyntax: Syntax[String, Char, Char, WeekDay] = Syntax.oneOf(
      mondaySyntax,
      tuesdaySyntax,
      wednesdaySyntax,
      thursdaySyntax,
      fridaySyntax,
      saturdaySyntax,
      sundaySyntax
    )
  }

  override def spec =
    suite("Syntax")(
      test("oneOf can parse sum types") {
        check(WeekDay.arbitrary) { day =>
          val parsed = WeekDay.weekDaySyntax.parseString(day.toString.take(3))
          assert(parsed)(isRight(equalTo(day)))
        }
      },
      test("oneOf can print sum types") {
        check(WeekDay.arbitrary) { day =>
          val printed = WeekDay.weekDaySyntax.printString(day)
          assert(printed)(isRight(equalTo(day.toString.take(3))))
        }
      },
      test("oneOf fails to parse garbage") {
        val parsed = WeekDay.weekDaySyntax.parseString("garbage")
        assert(parsed)(isLeft(isSubtype[AllBranchesFailed[String]](anything)))
      }
    )
}
