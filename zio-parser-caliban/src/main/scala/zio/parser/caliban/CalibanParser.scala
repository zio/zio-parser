package zio.parser.caliban

import caliban.CalibanError.ParsingError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.DirectiveLocation._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension._
import caliban.parsing.adt.Definition._
import caliban.parsing.adt.Selection._
import caliban.parsing.adt.Type._
import caliban.parsing.adt._
import zio._
import zio.parser._
import zio.parser.caliban.CalibanParser.CalibanSyntax
import zio.parser.internal.Debug

object Numbers {
  val signedIntString = Syntax.digit.repeat.string
  val jsonNumber      = ZNumbers.jsonNumber
}

object CalibanParser {
  type CalibanSyntax[D1, D2] = Syntax[String, Char, Char, D1, D2]

  private def optColl[A, C[_] <: Seq[_]](c: C[A]): Option[C[A]] =
    if (c.nonEmpty) Some(c) else None

  private def optMap[K, V](c: Map[K, V]): Option[Map[K, V]] =
    if (c.nonEmpty) Some(c) else None

  lazy val UnicodeBOM                                                           = '\uFEFF'
  lazy val Tab                                                                  = '\u0009'
  lazy val Space                                                                = '\u0020'
  lazy val LF                                                                   = '\u000A'
  lazy val CR                                                                   = '\u000D'
  lazy val Comma                                                                = ','
  lazy val whitespace: CalibanSyntax[Char, Char]                                = Syntax.charIn(UnicodeBOM, Tab, Space, LF, CR, Comma)
  lazy val comment: Syntax[String, Char, Char, Chunk[Char], Unit]               =
    Syntax.char('#') ~> Syntax.anyChar.repeatUntil(Syntax.char(LF) | Syntax.string(s"$CR$LF", ())).unit
  lazy val whitespaceWithComment: Syntax[String, Char, Char, Unit, Unit]        = (whitespace | comment).*.unit(Chunk.empty)
  lazy val whitespaceWithComment1: Syntax[String, Char, Char, Unit, Unit]       = (whitespace | comment).+.unit(Chunk.empty)
  private def wrapBrackets[D1, D2](t: Syntax[String, Char, Char, D1, D2])       =
    (whitespaceWithComment ~> t <~ whitespaceWithComment).between(Syntax.char('{'), Syntax.char('}'))
  private def wrapParentheses[D1, D2](t: Syntax[String, Char, Char, D1, D2])    =
    (whitespaceWithComment ~> t <~ whitespaceWithComment).between(Syntax.char('('), Syntax.char(')'))
  private def wrapSquareBrackets[D1, D2](t: Syntax[String, Char, Char, D1, D2]) =
    Syntax.char('[').surroundedBy(whitespaceWithComment) ~> t <~ Syntax
      .char(']')
      .surroundedBy(whitespaceWithComment)
  private def wrapWhitespaces[D1, D2](t: Syntax[String, Char, Char, D1, D2])    =
    t.surroundedBy(whitespaceWithComment)

  private object StringUtil {
    lazy val decodeTable: Map[Char, Char] = Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', 8.toChar),  // backspace
      ('f', 12.toChar), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )

    lazy val escapedToken: Syntax[
      String,
      Char,
      Char,
      Char with (Char, Char) with (Char, Char, (Char, Char)) with (
          Char,
          Char,
          (Char, Char),
          (Char, Char, (Char, Char))
      ),
      Unit
    ] = {
      val escapes = Syntax.charIn(decodeTable.keys.toSeq: _*)

      val oct  = Syntax.charIn('0' to '7': _*)
      val octP = Syntax.char('o') ~ oct ~ oct

      val hex  = Syntax.charIn(('0' to '9') ++ ('a' to 'f') ++ ('A' to 'F'): _*)
      val hex2 = hex ~ hex
      val hexP = Syntax.char('x') ~ hex2

      val hex4 = hex2 ~ hex2
      val u4   = Syntax.char('u') ~ hex4
      val hex8 = hex4 ~ hex4
      val u8   = Syntax.char('U') ~ hex8

      val after = escapes | octP | hexP | u4 | u8
      (Syntax.char('\\') ~ after).unit
    }

    /** String content without the delimiter
      */
    def undelimitedString(endP: CalibanSyntax[Unit, Unit]): CalibanSyntax[String, String] =
      Syntax.anyChar
        .repeatUntil(endP)
//      escapedToken
//        .orElse((!endP) ~ Parser.filterChar(_ => true))
//        .+
        .string
        .transformEither(
          str =>
            unescape(str.dropRight(3)) match {
              case Right(str1) => Right(str1)
              case Left(_)     => Left("FAILED")
            },
          (str: String) => Right(str) // TODO: escape
        )

    lazy val simpleString: CalibanSyntax[String, String] =
      Syntax.filterChar(c => c >= ' ' && c != '"' && c != '\\', "Not a valid string character").repeat0.string

    def escapedString(q: Char): CalibanSyntax[String, String] = {
      val end = Syntax.char(q)
      end ~> (simpleString <~ end).orElse(undelimitedString(end) <~ end)
    }

    def unescape(str: String): Either[Int, String] = {
      val sb                  = new java.lang.StringBuilder
      def decodeNum(idx: Int, size: Int, base: Int): Int = {
        val end = idx + size
        if (end <= str.length) {
          val intStr = str.substring(idx, end)
          val asInt  =
            try Integer.parseInt(intStr, base)
            catch { case _: NumberFormatException => ~idx }
          sb.append(asInt.toChar)
          end
        } else ~str.length
      }
      @annotation.tailrec
      def loop(idx: Int): Int =
        if (idx >= str.length) {
          // done
          idx
        } else if (idx < 0) {
          // error from decodeNum
          idx
        } else {
          val c0 = str.charAt(idx)
          if (c0 != '\\') {
            sb.append(c0)
            loop(idx + 1)
          } else {
            // str(idx) == \
            val nextIdx = idx + 1
            if (nextIdx >= str.length) {
              // error we expect there to be a character after \
              ~idx
            } else {
              val c = str.charAt(nextIdx)
              decodeTable.get(c) match {
                case Some(d) =>
                  sb.append(d)
                  loop(idx + 2)
                case None    =>
                  c match {
                    case 'o'   => loop(decodeNum(idx + 2, 2, 8))
                    case 'x'   => loop(decodeNum(idx + 2, 2, 16))
                    case 'u'   => loop(decodeNum(idx + 2, 4, 16))
                    case 'U'   => loop(decodeNum(idx + 2, 8, 16))
                    case other =>
                      // \c is interpretted as just \c, if the character isn't escaped
                      sb.append('\\')
                      sb.append(other)
                      loop(idx + 2)
                  }
              }
            }
          }
        }

      val res = loop(0)
      if (res < 0) Left(~res)
      else Right(sb.toString)
    }
  }

  lazy val name: CalibanSyntax[String, String] =
    (Syntax.charIn(('a' to 'z') ++ ('A' to 'Z') ++ Seq('_'): _*) ~
      Syntax.charIn(('a' to 'z') ++ ('A' to 'Z') ++ Seq('_') ++ ('0' to '9'): _*).*).string

  lazy val booleanValue: CalibanSyntax[BooleanValue, BooleanValue] =
    Syntax.string("true", BooleanValue(true)) | Syntax.string("false", BooleanValue(false))

  lazy val intValue: CalibanSyntax[IntValue, IntValue] =
    (Numbers.signedIntString <~ (Syntax.char('.').not("Integer cannot have a dot")))
      .transform(
        IntValue(_),
        _.toBigInt.toString
      )

  lazy val floatValue: CalibanSyntax[FloatValue, FloatValue] = Numbers.jsonNumber.transform(
    FloatValue(_),
    _.toBigDecimal.toString()
  )

  lazy val stringValue: CalibanSyntax[StringValue, StringValue] =
    (
      (Syntax.string("\"\"\"", ()) ~> StringUtil
        .undelimitedString(endP = Syntax.string("\"\"\"", ()))
        .transform(
          blockStringValue(_),
          identity[String] // TODO: inverse of blockStringValue
        )) | StringUtil.escapedString('\"')
    ).transform(v => StringValue(v), _.value)

  private def blockStringValue(rawValue: String): String = {
    val l1           = rawValue.split("\r?\n").toList
    val commonIndent = l1 match {
      case Nil       => None
      case _ :: tail =>
        tail.foldLeft(Option.empty[Int]) { case (commonIndent, line) =>
          val indent = "[ \t]*".r.findPrefixOf(line).fold(0)(_.length)
          if (indent < line.length && commonIndent.fold(true)(_ > indent)) Some(indent) else commonIndent
        }
    }
    // remove indentation
    lazy val l2      = (commonIndent, l1) match {
      case (Some(value), head :: tail) => head :: tail.map(_.drop(value))
      case _                           => l1
    }
    // remove start lines that are only whitespaces
    lazy val l3      = l2.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty)
    // remove end lines that are only whitespaces
    lazy val l4      = l3.reverse.dropWhile("[ \t]*".r.replaceAllIn(_, "").isEmpty).reverse
    l4.mkString("\n")
  }

  lazy val nullValue: CalibanSyntax[NullValue.type, NullValue.type] = Syntax.string("null", NullValue)
  lazy val enumValue: CalibanSyntax[EnumValue, EnumValue]           =
    name.transform(EnumValue.apply, (value: EnumValue) => value.value)

  lazy val listValue: CalibanSyntax[ListValue, ListValue] =
    wrapSquareBrackets(value.repeatWithSep(whitespaceWithComment))
      .transform(
        values => ListValue(values.toList),
        (value: ListValue) => Chunk.fromIterable(value.values)
      )

  lazy val objectField: CalibanSyntax[(String, InputValue), (String, InputValue)] =
    (name <~ wrapWhitespaces(Syntax.char(':'))) ~ value

  lazy val objectValue: CalibanSyntax[ObjectValue, ObjectValue] =
    wrapBrackets(objectField.repeatWithSep0(whitespaceWithComment)).transform(
      values => ObjectValue(values.toMap),
      (value: ObjectValue) => Chunk.fromIterable(value.fields)
    )

  lazy val variable: CalibanSyntax[VariableValue, VariableValue] =
    (Syntax.char('$') ~> name).transform(
      VariableValue.apply,
      (value: VariableValue) => value.name
    )

  lazy val value: CalibanSyntax[InputValue, InputValue] =
    intValue.widen[InputValue] |
      floatValue.widen[InputValue] |
      booleanValue.widen[InputValue] |
      stringValue.widen[InputValue] |
      nullValue.widen[InputValue] |
      enumValue.widen[InputValue] |
      listValue.widen[InputValue] |
      objectValue.widen[InputValue] |
      variable.widen[InputValue]

  lazy val defaultValue: CalibanSyntax[InputValue, InputValue] = wrapWhitespaces(Syntax.char('=')) ~> value

  lazy val alias: CalibanSyntax[String, String] = name <~ whitespaceWithComment <~ Syntax.char(':')

  lazy val argument: CalibanSyntax[(String, InputValue), (String, InputValue)] =
    (name <~ wrapWhitespaces(Syntax.char(':'))) ~ value

  lazy val arguments: CalibanSyntax[Map[String, InputValue], Map[String, InputValue]] =
    wrapParentheses(argument.repeatWithSep0(whitespaceWithComment)).transform(
      v => v.toMap,
      (map: Map[String, InputValue]) => Chunk.fromIterable(map)
    )

  lazy val directive: CalibanSyntax[Directive, Directive]              =
    (Syntax.index ~ ((Syntax.char('@') ~> name) <~ whitespaceWithComment) ~ arguments.?).transform(
      { case (index, name, arguments) =>
        Directive(name, arguments.getOrElse(Map()), index)
      },
      (directive: Directive) => (directive.name, optMap(directive.arguments))
    )
  lazy val directives: CalibanSyntax[List[Directive], List[Directive]] =
    directive.repeatWithSep(whitespaceWithComment).toList

  lazy val selection: CalibanSyntax[Selection, Selection] =
    field.widen[Selection] | fragmentSpread.widen[Selection] | inlineFragment.widen[Selection]

  lazy val selectionSet: CalibanSyntax[List[Selection], List[Selection]] =
    wrapBrackets(selection.repeatWithSep0(whitespaceWithComment)).toList

  lazy val namedType: CalibanSyntax[NamedType, NamedType] =
    (name.filter((n: String) => n != "null", "Name cannot be 'null'") ~ Syntax.char('!').?).transform(
      { case (name, nonNull) =>
        NamedType(name, nonNull = nonNull.nonEmpty)
      },
      (named: NamedType) => (named.name, if (named.nonNull) Some('!') else None)
    )

  lazy val listType: CalibanSyntax[ListType, ListType] =
    (wrapSquareBrackets(type_) ~ Syntax.char('!').?).transform(
      { case (typ, nonNull) =>
        ListType(typ, nonNull = nonNull.nonEmpty)
      },
      (lt: ListType) => (lt.ofType, if (lt.nonNull) Some('!') else None)
    )

  lazy val type_ : CalibanSyntax[Type, Type] =
    namedType.widen[Type] | listType.widen[Type]

  lazy val argumentDefinition: CalibanSyntax[InputValueDefinition, InputValueDefinition]              =
    (((stringValue <~ whitespaceWithComment1).? ~ name <~ wrapWhitespaces(Syntax.char(':'))) ~
      (type_ <~ whitespaceWithComment) ~ ((defaultValue <~ whitespaceWithComment).? ~ directives.?)).transform(
      { case (description, name, type_, (defaultValue, directives)) =>
        InputValueDefinition(description.map(_.value), name, type_, defaultValue, directives.getOrElse(Nil))
      },
      (d: InputValueDefinition) =>
        (
          d.description.map(StringValue.apply),
          d.name,
          d.ofType,
          (d.defaultValue, optColl(d.directives))
        )
    )
  lazy val argumentDefinitions: CalibanSyntax[List[InputValueDefinition], List[InputValueDefinition]] =
    wrapParentheses(argumentDefinition.+).toList

  lazy val fieldDefinition: CalibanSyntax[FieldDefinition, FieldDefinition] =
    (((stringValue <~ whitespaceWithComment).? ~ (name <~ whitespaceWithComment)) ~
      (argumentDefinitions <~ whitespaceWithComment).? ~
      ((Syntax.char(':') <~ whitespaceWithComment) ~> type_ <~ whitespaceWithComment) ~ directives.?).transform(
      { case (description, name, args, type_, directives) =>
        FieldDefinition(description.map(_.value), name, args.getOrElse(Nil), type_, directives.getOrElse(Nil))
      },
      (d: FieldDefinition) =>
        (d.description.map(StringValue.apply), d.name, optColl(d.args), d.ofType, optColl(d.directives))
    )

  lazy val variableDefinition: CalibanSyntax[VariableDefinition, VariableDefinition] =
    ((variable <~ wrapWhitespaces(Syntax.char(':'))) ~
      (type_ <~ whitespaceWithComment) ~
      ((defaultValue <~ whitespaceWithComment).? ~ directives.?)).transform(
      { case (v, t, (default, dirs)) =>
        VariableDefinition(v.name, t, default, dirs.getOrElse(Nil))
      },
      (d: VariableDefinition) => (VariableValue(d.name), d.variableType, (d.defaultValue, optColl(d.directives)))
    )

  lazy val variableDefinitions: CalibanSyntax[Chunk[VariableDefinition], Chunk[VariableDefinition]] =
    wrapParentheses(variableDefinition.repeatWithSep0(whitespaceWithComment))

  lazy val field: CalibanSyntax[Field, Field] = (((Syntax.index ~ (alias <~ whitespaceWithComment).?) ~
    name <~ whitespaceWithComment) ~ (arguments <~ whitespaceWithComment).? ~
    (directives <~ whitespaceWithComment).? ~ selectionSet.?).transform(
    { case (index, alias, name, args, dirs, sels) =>
      Field(
        alias,
        name,
        args.getOrElse(Map()),
        dirs.getOrElse(Nil),
        sels.getOrElse(Nil),
        index
      )
    },
    (f: Field) => (f.alias, f.name, optMap(f.arguments), optColl(f.directives), optColl(f.selectionSet))
  )

  lazy val fragmentName: CalibanSyntax[String, String] =
    name.filter((n: String) => n != "on", "Fragment name cannot be 'on'")

  lazy val fragmentSpread: CalibanSyntax[FragmentSpread, FragmentSpread] =
    ((Syntax.string("...", ()) ~> fragmentName <~ whitespaceWithComment) ~ directives.?).transform(
      { case (name, dirs) =>
        FragmentSpread(name, dirs.getOrElse(Nil))
      },
      (spread: FragmentSpread) => (spread.name, optColl(spread.directives))
    )

  lazy val typeCondition: CalibanSyntax[NamedType, NamedType] =
    Syntax.string("on", ()) ~> whitespaceWithComment1 ~> namedType

  lazy val inlineFragment: CalibanSyntax[InlineFragment, InlineFragment] =
    (Syntax.string("...", ()) ~> whitespaceWithComment ~>
      (typeCondition <~ whitespaceWithComment).? ~ (directives <~ whitespaceWithComment).? ~ selectionSet).transform(
      { case (typeCondition, dirs, sel) =>
        InlineFragment(typeCondition, dirs.getOrElse(Nil), sel)
      },
      (f: InlineFragment) => (f.typeCondition, optColl(f.dirs), f.selectionSet)
    )

  lazy val operationType: CalibanSyntax[OperationType, OperationType] =
    Syntax.string[OperationType]("query", OperationType.Query) |
      Syntax.string[OperationType]("mutation", OperationType.Mutation) |
      Syntax.string[OperationType]("subscription", OperationType.Subscription)

  lazy val operationDefinition: CalibanSyntax[OperationDefinition, OperationDefinition] =
    ((operationType <~ whitespaceWithComment) ~ ((name <~ whitespaceWithComment).? ~
      (variableDefinitions <~ whitespaceWithComment).?) ~
      (directives <~ whitespaceWithComment).? ~ selectionSet).transform(
      { case (operationType, (name, variableDefinitions), directives, selection) =>
        OperationDefinition(
          operationType,
          name,
          variableDefinitions.map(_.toList).getOrElse(Nil),
          directives.getOrElse(Nil),
          selection
        )
      },
      (d: OperationDefinition) =>
        (
          d.operationType,
          (d.name, optColl(Chunk.fromIterable(d.variableDefinitions))),
          optColl(d.directives),
          d.selectionSet
        )
    ) |
      selectionSet
        .transform(
          selection => OperationDefinition(OperationType.Query, None, Nil, Nil, selection),
          (d: OperationDefinition) => d.selectionSet
        )

  lazy val fragmentDefinition: CalibanSyntax[FragmentDefinition, FragmentDefinition] =
    ((Syntax.string("fragment", ()) ~> whitespaceWithComment1 ~> fragmentName <~ whitespaceWithComment1) ~
      (typeCondition <~ whitespaceWithComment) ~ (directives <~ whitespaceWithComment).? ~ selectionSet).transform(
      { case (name, typeCondition, dirs, sel) =>
        FragmentDefinition(name, typeCondition, dirs.getOrElse(Nil), sel)
      },
      (d: FragmentDefinition) => (d.name, d.typeCondition, optColl(d.directives), d.selectionSet)
    )

  private def objectTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[ObjectTypeDefinition, ObjectTypeDefinition] =
    ((Syntax.string("type", ()) ~> whitespaceWithComment1 ~> name <~ whitespaceWithComment1) ~
      ((implements <~ whitespaceWithComment).? ~ (directives <~ whitespaceWithComment).?) ~
      wrapBrackets(fieldDefinition.repeatWithSep0(whitespaceWithComment))).transform(
      { case (name, (implements, directives), fields) =>
        ObjectTypeDefinition(
          description,
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.toList
        )
      },
      (d: ObjectTypeDefinition) =>
        (d.name, (optColl(d.implements), optColl(d.directives)), Chunk.fromIterable(d.fields))
    )

  lazy val implements: CalibanSyntax[List[NamedType], List[NamedType]] =
    (((Syntax.string("implements", ()) <~ whitespaceWithComment <~
      (Syntax.char('&') <~ whitespaceWithComment).?.unit(None)) ~> namedType <~ whitespaceWithComment) ~
      (Syntax.char('&') ~> whitespaceWithComment ~> namedType).repeatWithSep0(whitespaceWithComment)).transform(
      { case (name, tps) => (name :: tps.toList) },
      tps => (tps.head, Chunk.fromIterable(tps.tail))
    )

  private def interfaceTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[InterfaceTypeDefinition, InterfaceTypeDefinition] =
    ((Syntax.string("interface", ()) ~> whitespaceWithComment1 ~> name <~ whitespaceWithComment) ~
      (directives <~ whitespaceWithComment).? ~ wrapBrackets(
        fieldDefinition.repeatWithSep0(whitespaceWithComment)
      )).transform(
      { case (name, directives, fields) =>
        InterfaceTypeDefinition(description, name, directives.getOrElse(Nil), fields.toList)
      },
      (d: InterfaceTypeDefinition) => (d.name, optColl(d.directives), Chunk.fromIterable(d.fields))
    )

  private def inputObjectTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[InputObjectTypeDefinition, InputObjectTypeDefinition] =
    ((Syntax.string(
      "input",
      ()
    ) ~> whitespaceWithComment1 ~> name <~ whitespaceWithComment) ~ (directives <~ whitespaceWithComment).? ~
      wrapBrackets(argumentDefinition.repeatWithSep0(whitespaceWithComment))).transform(
      { case (name, directives, fields) =>
        InputObjectTypeDefinition(description, name, directives.getOrElse(Nil), fields.toList)
      },
      (d: InputObjectTypeDefinition) => (d.name, optColl(d.directives), Chunk.fromIterable(d.fields))
    )

  lazy val enumValueDefinition: CalibanSyntax[EnumValueDefinition, EnumValueDefinition] =
    ((stringValue <~ whitespaceWithComment).? ~ (name <~ whitespaceWithComment) ~ directives.?).transform(
      { case (description, enumValue, directives) =>
        EnumValueDefinition(description.map(_.value), enumValue, directives.getOrElse(Nil))
      },
      (d: EnumValueDefinition) => (d.description.map(StringValue.apply), d.enumValue, optColl(d.directives))
    )

  lazy val enumName: CalibanSyntax[String, String] =
    name.filter(s => s != "true" && s != "false" && s != "null", "Enum name cannot be 'true', 'false' or 'null'")

  private def enumTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[EnumTypeDefinition, EnumTypeDefinition] =
    ((Syntax.string("enum", ()) ~> whitespaceWithComment1 ~> enumName <~ whitespaceWithComment) ~
      (directives <~ whitespaceWithComment).? ~ wrapBrackets(
        enumValueDefinition.repeatWithSep0(whitespaceWithComment)
      )).transform(
      { case (name, directives, enumValuesDefinition) =>
        EnumTypeDefinition(description, name, directives.getOrElse(Nil), enumValuesDefinition.toList)
      },
      (d: EnumTypeDefinition) => (d.name, optColl(d.directives), Chunk.fromIterable(d.enumValuesDefinition))
    )

  private def unionTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[UnionTypeDefinition, UnionTypeDefinition] = {
    val parser0: Syntax[
      String,
      Char,
      Char,
      (String, Option[List[Directive]], NamedType, Chunk[NamedType]),
      (String, Option[List[Directive]], NamedType, Chunk[NamedType])
    ] =
      (Syntax.string("union", ()) ~> whitespaceWithComment1 ~> name <~ whitespaceWithComment) ~
        ((directives <~ whitespaceWithComment).? <~ Syntax.char('=') <~ whitespaceWithComment) ~
        ((Syntax.char('|') <~ whitespaceWithComment).?.unit(None) ~> namedType <~ whitespaceWithComment) ~
        ((Syntax.char('|') <~ whitespaceWithComment) ~> namedType).repeatWithSep(whitespaceWithComment)

    parser0.transform(
      { result =>
        println(s"RESULT $result")
        result match {
          case okay @ (name, directives, m, ms) =>
            println(s"OKAY $okay")
            UnionTypeDefinition(description, name, directives.getOrElse(Nil), (m :: ms.toList).map(_.name))
        }
      },
      (d: UnionTypeDefinition) =>
        (
          d.name,
          optColl(d.directives),
          NamedType(d.memberTypes.head, nonNull = false),
          Chunk.fromIterable(d.memberTypes.tail).map(NamedType(_, nonNull = false))
        )
    )
  }

  private def scalarTypeDefinition(
      description: Option[String]
  ): CalibanSyntax[ScalarTypeDefinition, ScalarTypeDefinition] =
    ((Syntax.string("scalar", ()) ~> whitespaceWithComment1 ~> name <~ whitespaceWithComment) ~ directives.?).transform(
      { case (name, directives) =>
        ScalarTypeDefinition(description, name, directives.getOrElse(Nil))
      },
      (d: ScalarTypeDefinition) => (d.name, optColl(d.directives))
    )

  lazy val rootOperationTypeDefinition: CalibanSyntax[(OperationType, NamedType), (OperationType, NamedType)] =
    (operationType <~ wrapWhitespaces(Syntax.char(':'))) ~ namedType

  lazy val schemaDefinition: CalibanSyntax[SchemaDefinition, SchemaDefinition] =
    ((Syntax.string("schema", ()) ~> whitespaceWithComment ~> (directives <~ whitespaceWithComment).?) ~
      wrapBrackets(rootOperationTypeDefinition.repeatWithSep0(whitespaceWithComment))).transform(
      { case (directives, ops) =>
        val opsMap = ops.toMap
        SchemaDefinition(
          directives.getOrElse(Nil),
          opsMap.get(OperationType.Query).map(_.name),
          opsMap.get(OperationType.Mutation).map(_.name),
          opsMap.get(OperationType.Subscription).map(_.name)
        )
      },
      (d: SchemaDefinition) =>
        (
          optColl(d.directives),
          Chunk.fromIterable(
            Seq(
              d.query.map(n => OperationType.Query -> NamedType(n, nonNull = false)),
              d.mutation.map(n => OperationType.Mutation -> NamedType(n, nonNull = false)),
              d.subscription.map(n => OperationType.Subscription -> NamedType(n, nonNull = false))
            ).flatten
          )
        )
    )

  lazy val schemaExtensionWithOptionalDirectivesAndOperations: CalibanSyntax[SchemaExtension, SchemaExtension] =
    ((directives <~ whitespaceWithComment).? ~
      wrapBrackets(rootOperationTypeDefinition.repeatWithSep0(whitespaceWithComment)).?).transform(
      { case (directives, ops) =>
        val opsMap = ops.getOrElse(Nil).toMap
        SchemaExtension(
          directives.getOrElse(Nil),
          opsMap.get(OperationType.Query).map(_.name),
          opsMap.get(OperationType.Mutation).map(_.name),
          opsMap.get(OperationType.Subscription).map(_.name)
        )
      },
      (e: SchemaExtension) =>
        (
          optColl(e.directives),
          optColl(
            Chunk.fromIterable(
              Seq(
                e.query.map(n => OperationType.Query -> NamedType(n, nonNull = false)),
                e.mutation.map(n => OperationType.Mutation -> NamedType(n, nonNull = false)),
                e.subscription.map(n => OperationType.Subscription -> NamedType(n, nonNull = false))
              ).flatten
            )
          )
        )
    )

  lazy val schemaExtension: CalibanSyntax[SchemaExtension, SchemaExtension] =
    Syntax.string("schema", ()) ~> whitespaceWithComment ~> schemaExtensionWithOptionalDirectivesAndOperations

  lazy val scalarTypeExtension: CalibanSyntax[ScalarTypeExtension, ScalarTypeExtension] =
    ((Syntax.string("scalar", ()) ~> whitespaceWithComment ~> name <~ whitespaceWithComment) ~ directives).transform(
      { case (name, directives) =>
        ScalarTypeExtension(name, directives)
      },
      (ste: ScalarTypeExtension) => (ste.name, ste.directives)
    )

  lazy val objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields
      : CalibanSyntax[ObjectTypeExtension, ObjectTypeExtension] =
    ((name <~ whitespaceWithComment) ~ ((implements <~ whitespaceWithComment).? ~
      (directives <~ whitespaceWithComment).?) ~
      wrapBrackets(fieldDefinition.repeatWithSep0(whitespaceWithComment)).?).transform(
      { case (name, (implements, directives), fields) =>
        ObjectTypeExtension(
          name,
          implements.getOrElse(Nil),
          directives.getOrElse(Nil),
          fields.map(_.toList).getOrElse(Nil)
        )
      },
      (ote: ObjectTypeExtension) =>
        (ote.name, (optColl(ote.implements), optColl(ote.directives)), optColl(Chunk.fromIterable(ote.fields)))
    )

  lazy val objectTypeExtension: CalibanSyntax[ObjectTypeExtension, ObjectTypeExtension] =
    Syntax.string("type", ()) ~> whitespaceWithComment1 ~>
      objectTypeExtensionWithOptionalInterfacesOptionalDirectivesAndFields

  lazy val interfaceTypeExtensionWithOptionalDirectivesAndFields
      : CalibanSyntax[InterfaceTypeExtension, InterfaceTypeExtension] =
    ((name <~ whitespaceWithComment) ~ ((directives <~ whitespaceWithComment).? ~
      wrapBrackets(fieldDefinition.repeatWithSep0(whitespaceWithComment)).?)).transform(
      { case (name, (directives, fields)) =>
        InterfaceTypeExtension(name, directives.getOrElse(Nil), fields.map(_.toList).getOrElse(Nil))
      },
      (ite: InterfaceTypeExtension) => (ite.name, (optColl(ite.directives), optColl(Chunk.fromIterable(ite.fields))))
    )

  lazy val interfaceTypeExtension: CalibanSyntax[InterfaceTypeExtension, InterfaceTypeExtension] =
    Syntax.string("interface", ()) ~> whitespaceWithComment1 ~>
      interfaceTypeExtensionWithOptionalDirectivesAndFields

  lazy val unionTypeExtensionWithOptionalDirectivesAndUnionMembers
      : CalibanSyntax[UnionTypeExtension, UnionTypeExtension] =
    ((name <~ whitespaceWithComment) ~
      ((directives <~ whitespaceWithComment).? <~ (Syntax.char('=') <~ whitespaceWithComment).?.unit(None)) ~
      ((Syntax.char('|') <~ whitespaceWithComment).?.unit(None) ~> (namedType <~ whitespaceWithComment).?) ~
      ((Syntax.char('|') <~ whitespaceWithComment) ~> namedType).repeatWithSep0(whitespaceWithComment))
      .transform(
        { case (name, directives, m, ms) =>
          val members = ms.toList
          UnionTypeExtension(name, directives.getOrElse(Nil), m.map(_ :: members).getOrElse(members).map(_.name))
        },
        (ute: UnionTypeExtension) =>
          (
            ute.name,
            optColl(ute.directives),
            ute.memberTypes.headOption.map(NamedType(_, nonNull = false)),
            Chunk.fromIterable(ute.memberTypes.tail).map(NamedType(_, nonNull = false))
          )
      )

  lazy val unionTypeExtension: CalibanSyntax[UnionTypeExtension, UnionTypeExtension] =
    Syntax.string("union", ()) ~> whitespaceWithComment1 ~>
      unionTypeExtensionWithOptionalDirectivesAndUnionMembers

  lazy val enumTypeExtensionWithOptionalDirectivesAndValues: CalibanSyntax[EnumTypeExtension, EnumTypeExtension] =
    ((enumName <~ whitespaceWithComment) ~ (directives <~ whitespaceWithComment).? ~
      wrapBrackets(enumValueDefinition.repeatWithSep0(whitespaceWithComment)).?).transform(
      { case (name, directives, enumValuesDefinition) =>
        EnumTypeExtension(name, directives.getOrElse(Nil), enumValuesDefinition.map(_.toList).getOrElse(Nil))
      },
      (ete: EnumTypeExtension) =>
        (ete.name, optColl(ete.directives), optColl(Chunk.fromIterable(ete.enumValuesDefinition)))
    )

  lazy val enumTypeExtension: CalibanSyntax[EnumTypeExtension, EnumTypeExtension] =
    Syntax.string("enum", ()) ~> whitespaceWithComment1 ~> enumTypeExtensionWithOptionalDirectivesAndValues

  lazy val inputObjectTypeExtensionWithOptionalDirectivesAndFields
      : CalibanSyntax[InputObjectTypeExtension, InputObjectTypeExtension] =
    ((name <~ whitespaceWithComment) ~ (directives <~ whitespaceWithComment).? ~
      wrapBrackets(argumentDefinition.repeatWithSep0(whitespaceWithComment)).?).transform(
      { case (name, directives, fields) =>
        InputObjectTypeExtension(name, directives.getOrElse(Nil), fields.map(_.toList).getOrElse(Nil))
      },
      (iote: InputObjectTypeExtension) =>
        (iote.name, optColl(iote.directives), optColl(Chunk.fromIterable(iote.fields)))
    )

  lazy val inputObjectTypeExtension: CalibanSyntax[InputObjectTypeExtension, InputObjectTypeExtension] =
    Syntax.string("input", ()) ~> whitespaceWithComment1 ~>
      inputObjectTypeExtensionWithOptionalDirectivesAndFields

  lazy val directiveLocation: CalibanSyntax[DirectiveLocation, DirectiveLocation] =
    Syntax.string[DirectiveLocation]("QUERY", ExecutableDirectiveLocation.QUERY) |
      Syntax.string[DirectiveLocation]("MUTATION", ExecutableDirectiveLocation.MUTATION) |
      Syntax.string[DirectiveLocation]("SUBSCRIPTION", ExecutableDirectiveLocation.SUBSCRIPTION) |
      Syntax.string[DirectiveLocation]("FIELD", ExecutableDirectiveLocation.FIELD) |
      Syntax.string[DirectiveLocation]("FRAGMENT_DEFINITION", ExecutableDirectiveLocation.FRAGMENT_DEFINITION) |
      Syntax.string[DirectiveLocation]("FRAGMENT_SPREAD", ExecutableDirectiveLocation.FRAGMENT_SPREAD) |
      Syntax.string[DirectiveLocation]("INLINE_FRAGMENT", ExecutableDirectiveLocation.INLINE_FRAGMENT) |
      Syntax.string[DirectiveLocation]("SCHEMA", TypeSystemDirectiveLocation.SCHEMA) |
      Syntax.string[DirectiveLocation]("SCALAR", TypeSystemDirectiveLocation.SCALAR) |
      Syntax.string[DirectiveLocation]("OBJECT", TypeSystemDirectiveLocation.OBJECT) |
      Syntax.string[DirectiveLocation]("FIELD_DEFINITION", TypeSystemDirectiveLocation.FIELD_DEFINITION) |
      Syntax.string[DirectiveLocation]("ARGUMENT_DEFINITION", TypeSystemDirectiveLocation.ARGUMENT_DEFINITION) |
      Syntax.string[DirectiveLocation]("INTERFACE", TypeSystemDirectiveLocation.INTERFACE) |
      Syntax.string[DirectiveLocation]("UNION", TypeSystemDirectiveLocation.UNION) |
      Syntax.string[DirectiveLocation]("ENUM", TypeSystemDirectiveLocation.ENUM) |
      Syntax.string[DirectiveLocation]("ENUM_VALUE", TypeSystemDirectiveLocation.ENUM_VALUE) |
      Syntax.string[DirectiveLocation]("INPUT_OBJECT", TypeSystemDirectiveLocation.INPUT_OBJECT) |
      Syntax.string[DirectiveLocation]("INPUT_FIELD_DEFINITION", TypeSystemDirectiveLocation.INPUT_FIELD_DEFINITION)

  lazy val directiveDefinition: CalibanSyntax[DirectiveDefinition, DirectiveDefinition] =
    ((stringValue <~ whitespaceWithComment).? ~
      (Syntax.string("directive @", ()) ~> name <~ whitespaceWithComment) ~
      ((argumentDefinitions <~ whitespaceWithComment).? <~ Syntax.string("on", ()) <~ whitespaceWithComment1) ~
      ((Syntax.char('|') <~ whitespaceWithComment).?.unit(None) ~> directiveLocation <~ whitespaceWithComment) ~
      (Syntax.char('|') ~> whitespaceWithComment ~> directiveLocation).repeatWithSep(whitespaceWithComment))
      .transform(
        { case (description, name, args, firstLoc, otherLoc) =>
          DirectiveDefinition(description.map(_.value), name, args.getOrElse(Nil), otherLoc.toList.toSet + firstLoc)
        },
        (d: DirectiveDefinition) =>
          (
            d.description.map(StringValue.apply),
            d.name,
            optColl(d.args),
            d.locations.head,
            Chunk.fromIterable(d.locations.tail)
          )
      )

  lazy val typeDefinition: CalibanSyntax[TypeDefinition, TypeDefinition] = {
    val parser: Parser[String, Char, TypeDefinition]                   = (stringValue <~ whitespaceWithComment).?.asParser.flatMap {
      stringValOpt =>
        val description = stringValOpt.map(_.value)

        objectTypeDefinition(description).widen[TypeDefinition].asParser |
          interfaceTypeDefinition(description).widen[TypeDefinition].asParser |
          inputObjectTypeDefinition(description).widen[TypeDefinition].asParser |
          enumTypeDefinition(description).widen[TypeDefinition].asParser |
          unionTypeDefinition(description).widen[TypeDefinition].asParser |
          scalarTypeDefinition(description).widen[TypeDefinition].asParser
    }
    val printer: Printer[String, Char, TypeDefinition, TypeDefinition] =
      Printer.byValue[String, Char, TypeDefinition, TypeDefinition] { (d: TypeDefinition) =>
        objectTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d) |
          interfaceTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d) |
          inputObjectTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d) |
          enumTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d) |
          unionTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d) |
          scalarTypeDefinition(d.description).widen[TypeDefinition].asPrinter(d)
      }

    parser <=> printer
  }

  lazy val typeSystemDefinition: CalibanSyntax[TypeSystemDefinition, TypeSystemDefinition] =
    typeDefinition.widen[TypeSystemDefinition] |
      schemaDefinition.widen[TypeSystemDefinition] |
      directiveDefinition.widen[TypeSystemDefinition]

  lazy val executableDefinition: CalibanSyntax[ExecutableDefinition, ExecutableDefinition] =
    operationDefinition.widen[ExecutableDefinition] | fragmentDefinition.widen[ExecutableDefinition]

  lazy val typeExtension: CalibanSyntax[TypeExtension, TypeExtension] =
    objectTypeExtension.widen[TypeExtension] |
      interfaceTypeExtension.widen[TypeExtension] |
      inputObjectTypeExtension.widen[TypeExtension] |
      enumTypeExtension.widen[TypeExtension] |
      unionTypeExtension.widen[TypeExtension] |
      scalarTypeExtension.widen[TypeExtension]

  lazy val typeSystemExtension: CalibanSyntax[TypeSystemExtension, TypeSystemExtension] =
    Syntax.string("extend ", ()) ~> (schemaExtension.widen[TypeSystemExtension] | typeExtension
      .widen[TypeSystemExtension])

  lazy val definition: CalibanSyntax[Definition, Definition] =
    executableDefinition.widen[Definition] | typeSystemDefinition.widen[Definition] | typeSystemExtension
      .widen[Definition]

  lazy val document: CalibanSyntax[ParsedDocument, ParsedDocument] =
    (whitespaceWithComment ~> definition.repeatWithSep(
      whitespaceWithComment
    ) <~ whitespaceWithComment <~ Syntax.end)
      .transform(
        seq => ParsedDocument(seq.toList),
        (doc: ParsedDocument) => Chunk.fromIterable(doc.definitions)
      )

  /** Parses the given string into a [[caliban.parsing.adt.Document]] object or fails with a
    * [[caliban.CalibanError.ParsingError]].
    */
  def parseQuery(query: String): IO[ParsingError, Document] = {
    val sm = SourceMapper(query)
    //    document.parse(query) match {
    //      case Left(error) =>
    //        IO.fail(ParsingError(error.toString, Some(sm.getLocation(error.failedAtOffset))))
    //      case Right(result) =>
    //        IO.succeed(Document(result._2.definitions,sm))
    //    }
    Task(document.parseString(query))
      .mapError(ex => ParsingError(s"Internal parsing error", innerThrowable = Some(ex)))
      .flatMap {
        case Left(error)   =>
          // IO.fail(ParsingError(error.toString, Some(sm.getLocation(error.failedAtOffset))))
          IO.fail(ParsingError(error.toString, None))
        case Right(result) =>
          IO.succeed(Document(result.definitions, sm))
      }
  }

  /** Checks if the query is valid, if not returns an error string.
    */
  def check(query: String): Option[String] = document.parseString(query) match {
    case Left(error) => Some(error.toString)
    case Right(_)    => None
  }
}

case class ParsedDocument(definitions: List[Definition], index: Int = 0)

object ZNumbers {

  /** a single base 10 digit
    */
  val digit: CalibanSyntax[Char, Char] = Syntax.digit

  /** zero or more digit chars
    */
  val digits0: CalibanSyntax[String, String] = digit.*.string

  /** one or more digit chars
    */
  val digits: CalibanSyntax[String, String] = digit.+.string

  /** a single base 10 digit excluding 0
    */
  val nonZeroDigit: CalibanSyntax[Char, Char] =
    Syntax.charIn('1' to '9': _*)

  /** A String of either 1 '0' or 1 non-zero digit followed by zero or more digits
    */
  val nonNegativeIntString: CalibanSyntax[String, String] =
    (nonZeroDigit ~ digits0).unit
      .orElse(Syntax.char('0'))
      .string

  /** A nonNegativeIntString possibly preceded by '-'
    */
  val signedIntString: CalibanSyntax[String, String] =
    (Syntax.char('-').? ~ nonNegativeIntString).string

  /** map a signedIntString into a BigInt
    */
  val bigInt: CalibanSyntax[String, BigInt] =
    signedIntString.map(BigInt(_))

  /** A string matching the json specification for numbers. from: https://tools.ietf.org/html/rfc4627
    */
  val jsonNumber: CalibanSyntax[String, String] = {
    /*
     *     number = [ minus ] int [ frac ] [ exp ]
     *     decimal-point = %x2E       ; .
     *     digit1-9 = %x31-39         ; 1-9
     *     e = %x65 / %x45            ; e E
     *     exp = e [ minus / plus ] 1*DIGIT
     *     frac = decimal-point 1*DIGIT
     *     int = zero / ( digit1-9 *DIGIT )
     *     minus = %x2D               ; -
     *     plus = %x2B                ; +
     *     zero = %x30                ; 0
     */
    val frac: CalibanSyntax[String, String]                    = Syntax.char('.') ~> digits
    val exp: CalibanSyntax[(Char, Option[Char], String), Unit] =
      (Syntax.charIn("eE") ~ Syntax.charIn("+-").? ~ digits).unit

    (signedIntString ~ frac.? ~ exp.?).string
  }

}

object CalibanDemo extends ZIOAppDefault {
  //  val query = """{
  //                |  human(id: COOL) {
  //                |    name
  //                |    height(unit: FOOT)
  //                |  }
  //                |}""".stripMargin

  val query = "\"\"\"\n  Hello,\n    World!\n\n  Yours,\n    GraphQL. \"\"\""
//  val query = "\"\"\"H\"\"\""
//  val query = "\"\"\"hello world\"\"\"".stripMargin

  val args: String =
    """
(id: "1000", int: 3, float: 3.14, bool: true, nope: null, enum: YES, list: [1,2,3])
""".trim

  val parsed: ZIO[Any, Nothing, Either[Parser.ParserError[String], StringValue]] =
    UIO(CalibanParser.stringValue.parseString(query))
      //  val parsed = UIO(CalibanSyntax.stringValue.parse("\"\"\"hello\"\"\""))
      .tap {
        case Left(value)  => UIO.unit
        case Right(value) =>
          UIO(value.getClass).debug("CLASS")
      }

//  Debug.printParserTree(CalibanParser.document.asParser.optimized)

  override def run: zio.URIO[Any, ExitCode] =
    parsed.debug("RESULT").exitCode
  //    parseQuery(query).debug("RESULT").exitCode
}
