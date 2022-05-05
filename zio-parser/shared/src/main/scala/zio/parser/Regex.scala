package zio.parser

import zio.Chunk
import zio.parser.Regex.Tabular.LookupFunction.Empty

import java.util.regex.Matcher
import scala.annotation.nowarn
import scala.collection.immutable.BitSet

/** A model of a regular expression.
  */
sealed trait Regex { self =>
  import Regex._

  /** Sequentially composes this regex with the specified regex, returning a regex that will first match this one, and
    * then match the specified regex.
    */
  def ~(that: Regex): Regex = Sequence(self, that)

  /** Composes this regex with the specified regex using intersection, returning a regex that will match a prefix only
    * if both this and the specified regex match it.
    */
  def &(that: Regex): Regex = And(self, that)

  /** Composes this regex with the specified regex using union, returning a regex that will match a prefix only if
    * either this or the specified regex match it.
    */
  def |(that: Regex): Regex = Or(self, that)

  /** Returns a new regex that matches at least `min` occurrences of this regex.
    */
  def atLeast(min: Int): Regex = Repeat(self, Some(min), None)

  /** Returns a new regex that matches at most `max` occurrences of this regex.
    */
  def atMost(max: Int): Regex = Repeat(self, None, Some(max))

  /** Returns a new regex that matches between `min` and `max` occurrences of this regex.
    */
  def between(min: Int, max: Int): Regex = Repeat(self, Some(min), Some(max))

  /** Compiles the regex to a form that allows efficient execution on chunks of characters.
    */
  def compile: Regex.Compiled = Parser(self)

  /** If the regex is a string literal, returns the string literal.
    */
  def toLiteral: Option[Chunk[Char]] = toLiteralChars(self)

  private def toLiteralChars(regex: Regex): Option[Chunk[Char]] =
    regex match {
      case Succeed                         => Some(Chunk.empty)
      case OneOf(chars) if chars.size == 1 => Some(Chunk(chars.head.toChar))
      case Sequence(first, second)         =>
        toLiteralChars(first).flatMap(first => toLiteralChars(second).map(second => first ++ second))
      case _                               => None
    }

  private def compileToTabular: Option[Compiled] =
    try Some(Tabular(self))
    catch {
      case _: IllegalArgumentException => None
    }
}

object Regex {
  private lazy val AllCharsBitSet = BitSet((Char.MinValue to Char.MaxValue).map(_.toInt): _*)

  final val NotMatched    = -1
  final val NeedMoreInput = -2

  /** A regex that matches at least one letter or digit character.
    */
  lazy val alphaNumerics: Regex = anyAlphaNumeric.atLeast(1)

  /** A regex that matches a single letter or digit character.
    */
  lazy val anyAlphaNumeric: Regex = anyLetter | anyDigit

  /** A regex that matches any single character.
    */
  lazy val anyChar: Regex = OneOf(AllCharsBitSet)

  /** A regex that matches any single digit character.
    */
  lazy val anyDigit: Regex = filter(_.isDigit)

  /** A regex that matches any single letter character.
    */
  lazy val anyLetter: Regex = filter(_.isLetter)

  /** A regex that matches any single whitespace character.
    */
  lazy val anyWhitespace: Regex = filter(_.isWhitespace)

  /** A regex that matches the specified character.
    */
  def char(char: Char): Regex = charIn(char)

  /** A regex that matches one of the specified characters.
    */
  def charIn(chars: Char*): Regex = OneOf(BitSet(chars.map(_.toInt): _*))

  /** A regex that matches any character except of the specified ones
    */
  def charNotIn(chars: Char*): Regex = OneOf(chars.foldLeft(AllCharsBitSet)((bs, ch) => bs - ch.toInt))

  /** A regex that matches one or more digit characters.
    */
  lazy val digits: Regex = filter(_.isDigit).atLeast(1)

  /** A regex that matches the empty string, which will always succeed.
    */
  val empty: Regex = Succeed

  /** A regex that matches any single character for which the specified predicate returns true.
    */
  def filter(p: Char => Boolean): Regex = charIn((Char.MinValue to Char.MaxValue).filter(p): _*)

  /** A regex that matches any one or more letter characters.
    */
  lazy val letters: Regex = anyLetter.atLeast(1)

  /** A regex that matches the specified literal string.
    */
  def string(token: String): Regex = token.toList.map(c => charIn(c)).foldLeft(empty)(_ ~ _)

  /** A regex that matches zero or more whitespace characters.
    */
  lazy val whitespace: Regex = anyWhitespace.atLeast(0)

  trait Compiled {

    /** Tests the compiled regex against the specified character sequence. Returns the new index into the chunk.
      */
    def test(index: Int, chars: String): Int

    /** Determines if the compiled regex matches the specified string.
      */
    final def matches(value: String): Boolean = test(0, value) >= 0
  }
  private[parser] case object Succeed                                                       extends Regex
  private[parser] final case class OneOf(bitset: BitSet)                                    extends Regex {
    def contains(char: Char): Boolean = bitset.contains(char.toInt)

    def isAllChars: Boolean = bitset == AllCharsBitSet
  }
  private[parser] final case class Sequence(first: Regex, second: Regex)                    extends Regex
  private[parser] final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex
  private[parser] final case class Or(left: Regex, right: Regex)                            extends Regex
  private[parser] final case class And(left: Regex, right: Regex)                           extends Regex

  /** Based on parser combinators, this implementation uses function composition for compilation. It has the advantage
    * of being very simple and reasonably performant. However, function composition is not safe for grammars of
    * arbitrary size, due to stack overflow exceptions. In addition, as designed here, these don't handle string
    * literals or alternations of string literals well.
    */
  private[parser] object Parser {
    def sequence(regex: Sequence): List[Regex] = {
      def loop(r: Regex): List[Regex] =
        r match {
          case Sequence(l, r) => loop(l) ++ loop(r)
          case r              => List(r)
        }

      loop(regex)
    }

    def compile(regex: Regex): (Int, String) => Int =
      regex.compileToTabular.map(compiled => compiled.test(_, _)).getOrElse {
        BuiltIn(regex).map(compiled => compiled.test(_, _)).getOrElse {
          regex match {
            case Succeed => (idx: Int, _: String) => idx

            case oneOf @ OneOf(_) =>
              (idx: Int, input: String) =>
                if (idx >= input.length) NeedMoreInput else if (oneOf.contains(input(idx))) idx + 1 else NotMatched

            case s @ Sequence(_, _) =>
              val compiled = Chunk.fromIterable(sequence(s).map(compile(_)))

              (idx0: Int, input: String) => {
                val compiledLen = compiled.length

                var i   = 0
                var idx = idx0

                while (i < compiledLen) {
                  val current = compiled(i)

                  idx = current(idx, input)

                  if (idx < 0) i = compiledLen // Terminate loop because current parser didn't match
                  else i = i + 1
                }

                idx
              }

            case Repeat(regex, min0, max0) =>
              val min = min0.getOrElse(0)
              val max = max0.getOrElse(Int.MaxValue)

              val compiled = compile(regex)

              (idx0: Int, input: String) => {
                val len = input.length

                var idx     = idx0
                var lastIdx = idx0
                var matched = 0

                while (idx >= 0 && idx < len && matched < max) {
                  idx = compiled(idx, input)

                  if (idx >= 0) {
                    lastIdx = idx
                    matched = matched + 1
                  }
                }

                if (matched < min) NeedMoreInput else lastIdx
              }

            case Or(left0, right0) =>
              val left  = compile(left0)
              val right = compile(right0)

              (idx: Int, input: String) =>
                left(idx, input) match {
                  case NotMatched    => right(idx, input)
                  case NeedMoreInput => right(idx, input)
                  case idx           => idx
                }

            case And(left0, right0) =>
              val left  = compile(left0)
              val right = compile(right0)

              (idx: Int, input: String) =>
                left(idx, input) match {
                  case NotMatched    => NotMatched
                  case NeedMoreInput => NeedMoreInput
                  case _             => right(idx, input)
                }
          }
        }
      }

    def apply(regex: Regex): Compiled =
      new Compiled {
        val compiled = compile(regex)

        def test(index: Int, input: String): Int =
          compiled(index, input)
      }
  }

  /** A regex compiler that uses tables to do all the lifting.
    */
  private[parser] object Tabular {
    def apply(regex: Regex): Tabular =
      regex match {
        case Succeed => Empty

        case oneOf @ OneOf(chars) =>
          if (oneOf.isAllChars) {
            LookupFunction.AcceptAll
          } else {
            val array = Array.fill[Step](if (chars.exists(_ >= 256)) 65536 else 256)(Step.Error)

            chars.foreach(char => array(char.toInt) = Step.Matched)

            LookupFunction.Table(Chunk.fromArray(array))
          }

        case Sequence(first, second) => apply(first) ~ apply(second)

        case Repeat(regex0, min0, Some(max)) => // TODO: limit max to avoid SOE
          val min = min0.getOrElse(0)

          val regex = Tabular(regex0)

          val start = if (min == 0) Tabular(Succeed) else List.fill(min)(regex).reduce(_ ~ _)

          (min until max)
            .foldLeft((Set(start), start)) { case ((choices, current), _) =>
              val next = current ~ regex

              (choices + next, next)
            }
            ._1
            .reduce(_ | _)

        case Repeat(_, _, _) =>
          throw new IllegalArgumentException("Cannot compile to DFA unbounded repetition")

        case Or(left, right) => apply(left) | apply(right)

        case And(left, right) => apply(left) & apply(right)
      }

    sealed trait Step { self =>
      import Step._

      def ~(that: Step): Step =
        (self, that) match {
          case (Matched, right) => right
          case (left, Matched)  => left

          case (Jump(left), Jump(right)) => Jump(left ~ right)

          case (MatchedOrJump(left), Jump(right)) => Jump((left ~ right) | right)

          case _ => Error
        }

      def &(that: Step): Step =
        (self, that) match {
          case (Matched, Matched)        => Matched
          case (Jump(left), Jump(right)) => Jump(left & right)
          case _                         => Error
        }

      def |(that: Step): Step =
        (self, that) match {
          case (Matched, Jump(next))                       => MatchedOrJump(next)
          case (Jump(next), Matched)                       => MatchedOrJump(next)
          case (_, Matched)                                => Matched
          case (Matched, _)                                => Matched
          case (left, Error)                               => left
          case (Error, right)                              => right
          case (Jump(left), Jump(right))                   => Jump(left | right)
          case (MatchedOrJump(left), Jump(right))          => MatchedOrJump(left | right)
          case (Jump(left), MatchedOrJump(right))          => MatchedOrJump(left | right)
          case (MatchedOrJump(left), MatchedOrJump(right)) => MatchedOrJump(left | right)
          case _                                           => Error
        }

      override def toString(): String = self match {
        case Matched          => "Matched"
        case Error            => "Error"
        case MatchedOrJump(_) => "MatchedOrJump(<lookup>)"
        case Jump(_)          => "Jump(<lookup>)"
      }
    }
    object Step       {
      case object Matched                                    extends Step
      case object Error                                      extends Step
      final case class MatchedOrJump(lookup: LookupFunction) extends Step
      final case class Jump(lookup: LookupFunction)          extends Step
    }

    sealed trait Tabular extends Compiled { self =>
      def ~(that: Tabular): Tabular =
        (self, that) match {
          case (Empty, lookup: LookupFunction)               => lookup
          case (lookup: LookupFunction, Empty)               => lookup
          case (left: LookupFunction, right: LookupFunction) => (left ~ right): LookupFunction
          case _                                             => Empty
        }

      def |(that: Tabular): Tabular =
        (self, that) match {
          case (left: LookupFunction, right: LookupFunction) => (left | right): LookupFunction
          case _                                             => Empty
        }

      def &(that: Tabular): Tabular =
        (self, that) match {
          case (Empty, lookup: LookupFunction)               => lookup
          case (lookup: LookupFunction, Empty)               => lookup
          case (left: LookupFunction, right: LookupFunction) => (left & right): LookupFunction
          case _                                             => Empty
        }
    }

    sealed trait LookupFunction extends Tabular { self =>
      def apply(char: Int): Step

      def supportsEmpty: Boolean

      def ~(that: LookupFunction): LookupFunction

      def |(that: LookupFunction): LookupFunction

      def &(that: LookupFunction): LookupFunction

      def test(index: Int, input: String): Int = {
        var curLookup = self
        var curIdx    = index
        val inputLen  = input.length
        var returnV   = NeedMoreInput

        while (curIdx < inputLen) {
          val char = input(curIdx).toInt

          curIdx = curIdx + 1

          curLookup(char) match {
            case Step.Matched                           => returnV = curIdx; curIdx = inputLen;
            case Step.Error if returnV == NeedMoreInput => returnV = NotMatched; curIdx = inputLen;
            case Step.Error                             => curIdx = inputLen;
            case Step.Jump(lookup)                      => curLookup = lookup
            case Step.MatchedOrJump(lookup)             => returnV = curIdx; curLookup = lookup;
          }
        }

        if ((returnV == NeedMoreInput || returnV == NotMatched) && self.supportsEmpty) {
          index
        } else {
          returnV
        }
      }
    }
    object LookupFunction {
      abstract class ComputedLookupFunction extends LookupFunction { self =>
        final def ~(that: LookupFunction): LookupFunction = LookupFunction.Seq(self, that)

        final def |(that: LookupFunction): LookupFunction = LookupFunction.Or(self, that)

        final def &(that: LookupFunction): LookupFunction = LookupFunction.And(self, that)
      }

      case object AcceptAll                                               extends ComputedLookupFunction {
        def apply(char: Int): Step = Step.Matched
        val supportsEmpty: Boolean = false
      }
      final case class And(left: LookupFunction, right: LookupFunction)   extends ComputedLookupFunction {
        def apply(char: Int): Step = left(char) & right(char)
        val supportsEmpty: Boolean = left.supportsEmpty && right.supportsEmpty
      }
      final case class Or(left: LookupFunction, right: LookupFunction)    extends ComputedLookupFunction {
        def apply(char: Int): Step = left(char) | right(char)
        val supportsEmpty: Boolean = left.supportsEmpty || right.supportsEmpty
      }
      final case class Seq(first: LookupFunction, second: LookupFunction) extends ComputedLookupFunction {
        val next = if (second.supportsEmpty) {
          Step.MatchedOrJump(second)
        } else {
          Step.Jump(second)
        }

        def apply(char: Int): Step =
          first(char) ~ next

        val supportsEmpty: Boolean = first.supportsEmpty && second.supportsEmpty
      }

      /** A degenerate tabular result that can succeed without computing any input.
        */
      case object Empty extends ComputedLookupFunction {
        def apply(char: Int): Step = Step.Error
        val supportsEmpty: Boolean = true
      }

      final case class Table(parseChar: Chunk[Step]) extends LookupFunction { self =>
        private[this] val len = parseChar.length

        val supportsEmpty: Boolean = false

        override def ~(that: LookupFunction): LookupFunction =
          that match {
            case that if that.supportsEmpty => copy(parseChar = parseChar.map(_ ~ Step.MatchedOrJump(that)))
            case that                       => copy(parseChar = parseChar.map(_ ~ Step.Jump(that)))
          }

        override def |(that: LookupFunction): LookupFunction =
          that match {
            case that @ Table(_) => self.combineWith(that)(_ | _)
            case that            => Or(self, that)
          }

        override def &(that: LookupFunction): LookupFunction =
          that match {
            case that @ Table(_) => self.combineWith(that)(_ & _)
            case that            => And(self, that)
          }

        def combineWith(that: Table)(f: (Step, Step) => Step): Table = {
          val (self1, that1) = self.equalize(that)

          Table(self1.parseChar.zip(that1.parseChar).map(f.tupled))
        }

        private def equalize(that: Table): (Table, Table) =
          if (that.parseChar.length > self.parseChar.length) {
            val extension = Chunk.fill(that.parseChar.length - self.parseChar.length)(Step.Error)

            (Table(self.parseChar ++ extension), that)
          } else if (self.parseChar.length > that.parseChar.length) {
            val extension = Chunk.fill(self.parseChar.length - that.parseChar.length)(Step.Error)

            (self, Table(that.parseChar ++ extension))
          } else (self, that)

        def apply(char: Int): Step = if (char >= len) Step.Error else parseChar(char)
      }
    }
  }

  private[parser] object BuiltIn {
    @nowarn def apply(regex: Regex): Option[BuiltIn] =
      None // NOTE: enabling it makes the parser non-threadsafe
//      val builder = new StringBuilder
//      builder.append("\\G")
//      if (toRegexString(regex, builder)) {
//        Some(
//          BuiltIn(
//            new matching.Regex(builder.toString())
//          )
//        )
//      } else None

    private def unicodeChar(ch: Char, builder: StringBuilder): Unit = {
      builder.append("\\x{%02x}".format(ch.toInt))
      ()
    }

    private def emitRegion(regionStart: Option[Int], last: Option[Int], builder: StringBuilder): Unit =
      regionStart match {
        case Some(start) =>
          val end = last.get

          if (start == end) {
            unicodeChar(start.toChar, builder)
          } else {
            unicodeChar(regionStart.get.toChar, builder)
            builder.append('-')
            unicodeChar(last.get.toChar, builder)
          }
        case None        =>
          unicodeChar(last.get.toChar, builder)
      }

    private def toRegexString(regex: Regex, builder: StringBuilder): Boolean =
      regex match {
        case Succeed                 =>
          true
        case OneOf(bitset)           =>
          if (bitset.isEmpty) {
            builder.append("[^.]")
          } else if (bitset.size == AllCharsBitSet.size) {
            builder.append(".")
          } else if (bitset.size == 1) {
            unicodeChar(bitset.head.toChar, builder)
          } else {
            builder.append('[')

            var regionStart: Option[Int] = None
            var last: Option[Int]        = None

            for (int <- bitset)
              if (last.contains(int - 1)) {
                last = Some(int)
              } else {
                if (last.isDefined)
                  emitRegion(regionStart, last, builder)
                last = Some(int)
                regionStart = Some(int)
              }
            if (last.isDefined)
              emitRegion(regionStart, last, builder)
            builder.append(']')
          }
          true
        case Sequence(first, second) =>
          val r1 = toRegexString(first, builder)
          val r2 = toRegexString(second, builder)
          r1 && r2
        case Repeat(Succeed, _, _)   =>
          true
        case Repeat(regex, min, max) =>
          val r = toRegexString(regex, builder)
          (min, max) match {
            case (None, None)                         =>
              builder.append('*')
            case (None, Some(1))                      =>
              builder.append('?')
            case (None, Some(max))                    =>
              builder.append(s"{0,$max}")
            case (Some(min), Some(max)) if min == max =>
              builder.append(s"{$min}")
            case (Some(min), Some(max))               =>
              builder.append(s"{$min,$max}")
            case (Some(0), None)                      =>
              builder.append('*')
            case (Some(1), None)                      =>
              builder.append('+')
            case (Some(min), None)                    =>
              builder.append(s"{$min,}")
          }
          r
        case Or(left, right)         =>
          builder.append('(')
          val r1 = toRegexString(left, builder)
          builder.append('|')
          val r2 = toRegexString(right, builder)
          builder.append(')')
          r1 && r2
        case And(_, _)               =>
          false
      }

    case class BuiltIn(regex: scala.util.matching.Regex) extends Regex.Compiled {
      private val matcher: Matcher = regex.pattern.matcher("")

      /** Tests the compiled regex against the specified character sequence. Returns the new index into the chunk.
        */
      override def test(index: Int, chars: String): Int = {
        matcher.reset(chars)
        if (matcher.find(index))
          matcher.end()
        else {
          Regex.NotMatched
        }
      }
    }
  }
}
