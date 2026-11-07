---
id: regex
title: "Regex"
---

Character-level syntaxes such as `Syntax.digit` or `Syntax.string` are built on top of `Regex`, a small model of regular expressions that ZIO Parser compiles to an efficient matcher. You can use it directly when you need to match a whole token in a single step instead of composing it from many small syntaxes.

```scala mdoc:silent
import zio.Chunk
import zio.parser.*
```

## Building a `Regex`

`Regex` is not `java.util.regex.Pattern`. It is a set of combinators:

| Combinator                              | Description                                                   |
|-----------------------------------------|---------------------------------------------------------------|
| `Regex.char(c)`, `Regex.string(s)`      | Match a character or a literal string                         |
| `Regex.charIn(cs*)`, `Regex.charNotIn(cs*)` | Match a character in or not in a set                      |
| `Regex.filter(p)`                       | Match one character that satisfies a predicate                |
| `Regex.anyChar`, `Regex.anyDigit`, `Regex.anyLetter`, `Regex.anyAlphaNumeric`, `Regex.anyWhitespace` | Match one character of a class |
| `Regex.digits`, `Regex.letters`, `Regex.alphaNumerics`, `Regex.whitespace` | Repeated classes (`whitespace` matches zero or more) |
| `Regex.empty`                           | Matches the empty string                                      |
| `a ~ b`                                 | `a` followed by `b`                                           |
| `a \| b`                                | Either `a` or `b`                                             |
| `a & b`                                 | Input matched by both `a` and `b`                             |
| `a.atLeast(n)`, `a.atMost(n)`, `a.between(min, max)` | Repetition                                       |

```scala mdoc:silent
val identifier: Regex =
  Regex.anyLetter ~ (Regex.anyLetter | Regex.anyDigit | Regex.char('_')).atLeast(0)

val hexByte: Regex =
  Regex.charIn("0123456789abcdefABCDEF".toList*).between(2, 2)
```

A regex can be compiled and tested on its own:

```scala mdoc
val compiled = identifier.compile
compiled.matches("user_42")
compiled.matches("42user")
```

## Regex-based Syntaxes

The `Syntax.regex*` constructors turn a `Regex` into a syntax. They differ in what the syntax results in:

| Constructor                                      | Value          | Description                                                           |
|--------------------------------------------------|----------------|-----------------------------------------------------------------------|
| `Syntax.regex(regex, failure)`                   | `Chunk[Char]`  | The matched characters                                                |
| `Syntax.regexChar(regex, failure)`               | `Char`         | The last matched character. Use it for regexes that match one character |
| `Syntax.regexDiscard(regex, failure, printed)`   | `Unit`         | Discards the match. When printing, `printed` is written               |

The `unsafeRegex`, `unsafeRegexChar` and `unsafeRegexDiscard` variants have no failure value and must only be used with regexes that cannot fail.

```scala mdoc:silent
val identifierSyntax: Syntax[String, Char, Char, String] =
  Syntax.regex(identifier, "not an identifier").string

val whitespace: Syntax[Nothing, Char, Char, Unit] =
  Syntax.unsafeRegexDiscard(Regex.whitespace, Chunk(' '))
```

```scala mdoc
identifierSyntax.parseString("count_1")
identifierSyntax.parseString("1count")
whitespace.parseString("   ")
whitespace.printString(())
```

`whitespace` accepts any amount of whitespace when parsing, and prints a single space.

When printing, the regex acts as a validator: a `Chunk[Char]` that the regex does not match makes the printer fail with the failure value.

```scala mdoc
Syntax.regex(identifier, "not an identifier").printString(Chunk.fromIterable("abc".toList))
Syntax.regex(identifier, "not an identifier").printString(Chunk.fromIterable("1bc".toList))
```

## Why Use Regexes?

Each `Regex` is compiled once into a table-driven matcher that runs over the input in a tight loop. A syntax such as `Syntax.digit.repeat` is made of many parsing steps, while `Syntax.regex(Regex.digits, ...)` checks the whole token in one step. Prefer regexes for lexical tokens, like numbers, identifiers and whitespace, and combinators for structure.

The built-in `Syntax.digit`, `Syntax.letter`, `Syntax.alphaNumeric` and `Syntax.whitespace` are themselves defined with `Syntax.regexChar`.

Next: [Printing](printing.md).
