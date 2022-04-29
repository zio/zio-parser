package zio.parser

import zio.parser.SyntaxSpec.WeekDay

object Fuck {
  import WeekDay._
  val weekDaySyntax = Syntax.oneOf[WeekDay](
    mondaySyntax,
    tuesdaySyntax,
    wednesdaySyntax,
    thursdaySyntax,
    fridaySyntax,
    saturdaySyntax,
    sundaySyntax
  )
}
