---
id: syntax
title: "Building Syntaxes"
---

A `Syntax[Err, In, Out, Value]` describes a grammar once and gives you both directions: a parser that turns input into a `Value`, and a printer that turns a `Value` back into output. This page covers the combinators you use to build syntaxes.

```scala mdoc:silent
import zio.Chunk
import zio.parser.*
```

## The `Syntax` Type

`Syntax` has four type parameters:

| Parameter | Meaning                                                                        |
|-----------|--------------------------------------------------------------------------------|
| `Err`     | The custom error type, for example `String`                                    |
| `In`      | The element type of the input stream. For text this is `Char`                  |
| `Out`     | The element type of the printed stream. For text this is `Char`                |
| `Value`   | The type of the parsed value, which is also the type of the value to be printed |

Every syntax exposes its two halves as `asParser` and `asPrinter`, but you normally work with the syntax itself and call `parseString` or `printString` on it.

## Primitives

`Syntax` provides constructors for single characters, strings and common character classes:

| Constructor                                | Value    | Description                                       |
|--------------------------------------------|----------|---------------------------------------------------|
| `Syntax.char(c)`                           | `Unit`   | Matches the exact character `c`                   |
| `Syntax.charIn(cs)` / `Syntax.charNotIn(cs)` | `Char` | Matches a character in (or not in) the given set  |
| `Syntax.notChar(c)`                        | `Char`   | Matches any character except `c`                  |
| `Syntax.anyChar`                           | `Char`   | Matches any single character                      |
| `Syntax.digit`                             | `Char`   | Matches a digit                                   |
| `Syntax.letter`                            | `Char`   | Matches a letter                                  |
| `Syntax.alphaNumeric`                      | `Char`   | Matches a letter or a digit                       |
| `Syntax.whitespace`                        | `Char`   | Matches a whitespace character                    |
| `Syntax.filterChar(p, failure)`            | `Char`   | Matches a character that satisfies `p`            |
| `Syntax.string(s, value)`                  | `Result` | Matches the exact string `s` and results in `value` |
| `Syntax.succeed(v)` / `Syntax.fail(e)`     |          | Always succeed with `v` / always fail with `e`    |
| `Syntax.end`                               | `Unit`   | Succeeds only at the end of the input             |

```scala mdoc:silent
val comma: Syntax[String, Char, Char, Unit]       = Syntax.char(',')
val vowel: Syntax[String, Char, Char, Char]       = Syntax.charIn("aeiou")
val trueSyntax: Syntax[String, Char, Char, Boolean] = Syntax.string("true", true)
```

```scala mdoc
vowel.parseString("a")
vowel.parseString("x")
trueSyntax.parseString("true")
```

## Sequencing

The `~` operator runs two syntaxes in order and combines their results into a tuple. Results of type `Unit` are dropped, so you do not end up with `Unit` entries in the tuple:

```scala mdoc:silent
val pair: Syntax[String, Char, Char, (Char, Char)] =
  Syntax.digit ~ comma ~ Syntax.letter
```

```scala mdoc
pair.parseString("1,a")
pair.printString(('1', 'a'))
```

If you only care about one side, use `~>` to keep the right result or `<~` to keep the left result. The discarded side must be a `Syntax[..., Unit]`, because the printer has no value to give it:

```scala mdoc:silent
val parenthesizedDigit: Syntax[String, Char, Char, Char] =
  Syntax.char('(') ~> Syntax.digit <~ Syntax.char(')')
```

```scala mdoc
parenthesizedDigit.parseString("(7)")
parenthesizedDigit.printString('7')
```

## Alternatives

`|` (or `orElse`, or its alias `<>`) tries the left syntax first and falls back to the right one if it fails. Both sides must have the same value type. When the types differ, `orElseEither` (alias `<+>`) returns an `Either`:

```scala mdoc:silent
val boolean: Syntax[String, Char, Char, Boolean] =
  Syntax.string("true", true) | Syntax.string("false", false)

val digitOrLetter: Syntax[String, Char, Char, Either[Char, Char]] =
  Syntax.digit <+> Syntax.letter
```

```scala mdoc
boolean.parseString("false")
digitOrLetter.parseString("x")
```

When the alternatives are subtypes of a common type, `widen[T]` lifts each alternative to the parent type so they can be combined with `|`. The printer picks the first alternative that accepts the value. See the [worked example](example-json.md).

## Repetition

| Combinator                  | Result          | Description                                         |
|-----------------------------|-----------------|-----------------------------------------------------|
| `repeat` / `+`              | `Chunk[Value]`  | One or more times                                   |
| `repeat0` / `*`             | `Chunk[Value]`  | Zero or more times                                  |
| `atLeast(n)`                | `Chunk[Value]`  | At least `n` times                                  |
| `repeatWithSep(sep)`        | `Chunk[Value]`  | One or more times, separated by `sep`               |
| `repeatWithSep0(sep)`       | `Chunk[Value]`  | Zero or more times, separated by `sep`              |
| `repeatUntil(stop)`         | `Chunk[Value]`  | Repeats until `stop` succeeds                       |
| `optional` / `?`            | `Option[Value]` | Zero or one time                                    |

`string` turns a repeated character syntax into a `String`:

```scala mdoc:silent
val number: Syntax[String, Char, Char, String] = Syntax.digit.repeat.string
val numbers: Syntax[String, Char, Char, Chunk[String]] = number.repeatWithSep(comma)
val maybeSign: Syntax[String, Char, Char, Option[Unit]] = Syntax.char('-').optional
```

```scala mdoc
number.parseString("2024")
numbers.parseString("1,22,333")
numbers.printString(Chunk("4", "5", "6"))
maybeSign.parseString("")
```

`Syntax.digit.repeat` has `Chunk[Char]` as its value type, and `.string` replaces it with the matched text. When printing, the whole string is written as is.

## Surrounding Syntaxes

`between(left, right)` and `surroundedBy(other)` wrap a syntax with `Unit` syntaxes and keep the inner result:

```scala mdoc:silent
val quoted: Syntax[String, Char, Char, String] =
  Syntax.notChar('"').repeat0.string.surroundedBy(Syntax.char('"'))

val bracketed: Syntax[String, Char, Char, Chunk[String]] =
  number.repeatWithSep0(comma).between(Syntax.char('['), Syntax.char(']'))
```

```scala mdoc
quoted.parseString("\"hello\"")
bracketed.parseString("[1,2,3]")
bracketed.printString(Chunk("1", "2"))
```

## Mapping Values

Because a syntax works in both directions, a mapping needs a function for each direction:

| Combinator                       | Description                                                                                       |
|----------------------------------|---------------------------------------------------------------------------------------------------|
| `transform(to, from)`            | Maps the parsed value with `to`, and the value to print with `from`                               |
| `transformEither(to, from)`      | Both functions can fail with an `Either`                                                          |
| `transformOption(to, from)`      | Both functions can fail with an `Option`, and the error becomes `Option[Err]`                     |
| `transformTo(to, from, failure)` | `from` is a `PartialFunction`. If it is not defined for a value, printing fails with `failure`    |
| `asPrinted(result, value)`       | Always parses to `result`, and prints `value` when asked to print `result`                        |
| `as(value)`                      | For `Unit` syntaxes: result in `value` when parsing                                               |
| `of[CaseClass]` / `to[CaseClass]` | Converts a tuple result to a case class (and back for printing with `of`)                         |
| `filter(p, failure)`             | Fails with `failure` when the value does not satisfy `p`                                          |

```scala mdoc:silent
val int: Syntax[String, Char, Char, Int] =
  number.transformEither[String, Int](
    s => s.toIntOption.toRight("number out of range"),
    n => Right(n.toString)
  )

final case class Point(x: Int, y: Int)

val point: Syntax[String, Char, Char, Point] =
  (int ~ comma ~ int).of[Point].between(Syntax.char('('), Syntax.char(')'))
```

```scala mdoc
int.parseString("42")
int.parseString("99999999999")
point.parseString("(3,4)")
point.printString(Point(5, 6))
```

## Recursive Syntaxes

A syntax can refer to itself as long as the reference sits behind a `lazy val` (or a `def`). The `~`, `~>`, `<~`, `|` operators take their right-hand side by name, so the recursion is resolved lazily. For a full example see the [worked example](example-json.md).

```scala mdoc:silent
lazy val nested: Syntax[String, Char, Char, Int] =
  (Syntax.char('(') ~> nested <~ Syntax.char(')')).transform(_ + 1, _ - 1) |
    Syntax.succeed(0)
```

```scala mdoc
nested.parseString("((()))")
```

## Choosing the Parser Implementation

`parseString` and `parseChars` take an optional `ParserImplementation`. The default, `ParserImplementation.Recursive`, is fast but uses the JVM stack for nested syntaxes. `ParserImplementation.StackSafe` is slower but does not overflow on deeply nested input:

```scala mdoc
nested.parseString("(((())))", ParserImplementation.StackSafe)
```

## Backtracking

When a syntax fails after consuming input, the parser backtracks to where that syntax started, so that an alternative can try again from the same position. This automatic backtracking is on by default. `manualBacktracking` turns it off for a syntax, `autoBacktracking` turns it on again, and `backtrack` inserts an explicit backtracking point. Disabling it can make failures cheaper in grammars where you know which branch to take from the first character.

Next: [Regex](regex.md), [Printing](printing.md), [Error handling](errors.md).
