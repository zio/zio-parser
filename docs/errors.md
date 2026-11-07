---
id: errors
title: "Error Handling"
---

Parsing and printing both report failures as values. This page explains the error types, how to name the parts of a grammar so errors say where they happened, and how to turn errors into readable messages.

```scala mdoc:silent
import zio.Chunk
import zio.parser.*
import zio.parser.Parser.ParserError
```

## Error Types

`parseString` and `parseChars` return `Either[StringParserError[Err], Value]`. A `StringParserError` holds the original `input` and a `ParserError[Err]`. `parseChunk` returns the `ParserError[Err]` directly. A `ParserError` has these cases:

| Case                                  | Meaning                                                              |
|---------------------------------------|----------------------------------------------------------------------|
| `Failure(nameStack, position, failure)` | A syntax failed with your own error value of type `Err`            |
| `UnknownFailure(nameStack, position)` | A failure without a custom value (only from `unsafe*` syntaxes)     |
| `UnexpectedEndOfInput(nameStack)`     | The input ended before the syntax was complete                       |
| `NotConsumedAll(nameStack, position)` | `Syntax.end` found input left over                                   |
| `AllBranchesFailed(left, right)`      | Every alternative of a `\|` failed. Each branch's error is kept      |

`nameStack` lists the names of the enclosing named syntaxes (see [Naming Syntaxes](#naming-syntaxes)). `position` is the index in the input.

```scala mdoc:silent
val number: Syntax[String, Char, Char, Int] =
  Syntax.digit.repeat.string.transform(_.toInt, _.toString)

val pair: Syntax[String, Char, Char, (Int, Int)] =
  number ~ Syntax.char(',') ~ number
```

A syntax stops as soon as it has parsed a value and does not look at the rest of the input. To require that everything is consumed, finish with `Syntax.end`:

```scala mdoc:silent
val completePair: Syntax[String, Char, Char, (Int, Int)] = pair <~ Syntax.end
```

```scala mdoc
pair.parseString("1;2")
pair.parseString("1,")
pair.parseString("1,2 and more")
completePair.parseString("1,2 and more")
```

You can match on the error like any other value:

```scala mdoc
pair.parseString("1;2") match {
  case Left(StringParserError(_, ParserError.Failure(_, position, message))) =>
    s"$message at $position"
  case Left(other)  => other.toString
  case Right(value) => value.toString
}
```

## Pretty Printing Errors

`StringParserError#pretty` renders the error together with the relevant lines of the input and a caret under the failing position:

```scala mdoc
println(pair.parseString("1;2").left.map(_.pretty).merge)
```

Errors without a position, such as `UnexpectedEndOfInput` and `AllBranchesFailed`, are printed as a single line without the input. For a multi-line input only the lines around the error are shown.

## Custom Error Types

Most constructors default to a `String` error, and most also have a variant that takes your own failure value, for example `Syntax.char(c, failure)`, `Syntax.regex(regex, failure)` and `Syntax.filterChar(p, failure)`. Combine them with `mapError` to move to a richer error type:

```scala mdoc:silent
enum ConfigError {
  case BadNumber(text: String)
  case Other(message: String)
}

val port: Syntax[ConfigError, Char, Char, Int] =
  Syntax
    .digit
    .repeat
    .string
    .mapError(ConfigError.Other(_))
    .transformEither[ConfigError, Int](
      s => s.toIntOption.toRight(ConfigError.BadNumber(s)),
      n => Right(n.toString)
    )
```

```scala mdoc
port.parseString("8080")
port.parseString("99999999999")
```

`transformEither` and `filter` are the usual places to produce domain errors, while `transformOption` reports failures as `None`.

## Naming Syntaxes

Add a name to a syntax with `named` or its alias `??`. The name is pushed on the `nameStack` of any error raised inside, and `pretty` shows the chain:

```scala mdoc:silent
val namedNumber = number ?? "number"
val namedPair   = (namedNumber ~ Syntax.char(',') ~ namedNumber) ?? "pair"
```

```scala mdoc
println(namedPair.parseString("1;2").left.map(_.pretty).merge)
```

## Errors from Alternatives

With `|`, the parser tries each branch in order. If one succeeds, the failures of the earlier branches are discarded. If all fail, you get `AllBranchesFailed`, which `pretty` prints as `All branches failed: ... and ...`:

```scala mdoc:silent
val boolean: Syntax[String, Char, Char, Boolean] =
  Syntax.string("true", true) | Syntax.string("false", false)
```

```scala mdoc
boolean.parseString("maybe") match {
  case Left(StringParserError(_, ParserError.AllBranchesFailed(left, right))) =>
    (left.getClass.getSimpleName, right.getClass.getSimpleName)
  case other => other
}
```

## Backtracking and Errors

By default a failing branch of `|` rewinds the input, so the next branch starts at the same position. `manualBacktracking` disables that rewind for a syntax and `backtrack` restores it at selected points. See [Building Syntaxes](syntax.md#backtracking).

## Printing Errors

The `Err` type is shared between the parser and the printer, and printing returns `Either[Err, ...]`. A printer fails when a `filter` condition does not hold, when a `transformTo` partial function does not cover a value, or when a `widen` finds no matching alternative:

```scala mdoc
Syntax.digit.filter[String, Char](_ != '0', "no zeros").printString('0')
```

Next: the [worked example](example-json.md).
