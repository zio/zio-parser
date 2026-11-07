---
id: printing
title: "Printing and Invertible Syntaxes"
---

What sets ZIO Parser apart from most parser libraries is that the description of a grammar is *invertible*. The same `Syntax` value that parses a string can also print a value back to a string, so the two directions cannot drift apart.

```scala mdoc:silent
import zio.Chunk
import zio.parser.*
```

## Printing a Value

Every `Syntax` has three printing methods:

| Method                     | Result                    | Description                                      |
|----------------------------|---------------------------|--------------------------------------------------|
| `printString(value)`       | `Either[Err, String]`     | Prints to a `String` (requires `Out` to be `Char`) |
| `print(value)`             | `Either[Err, Chunk[Out]]` | Prints to a `Chunk` of output elements           |
| `print(value, target)`     | `Either[Err, Unit]`       | Writes to a [`Target`](#printing-to-a-target)    |

```scala mdoc:silent
val number: Syntax[String, Char, Char, Int] =
  Syntax.digit.repeat.string.transform(_.toInt, _.toString)

val range: Syntax[String, Char, Char, (Int, Int)] =
  number ~ Syntax.char('-') ~ number
```

```scala mdoc
range.parseString("10-20")
range.printString((10, 20))
range.print((1, 2))
```

## Round Trips

For a well-formed syntax, parsing the printed output gives back the original value:

```scala mdoc
range.printString((3, 9)).flatMap(range.parseString(_))
```

## Writing Syntaxes That Print

A parser only has to turn text into a value. A printer has to do the opposite, so every step that loses information on the way in needs a hint on how to restore it on the way out.

### Constant results

`as(value)` maps a `Unit` syntax to a constant. When printing, the printer is given that constant and writes the original text again:

```scala mdoc:silent
sealed trait Op
case object Plus  extends Op
case object Minus extends Op

val op: Syntax[String, Char, Char, Op] =
  Syntax.char('+').as(Plus: Op) | Syntax.char('-').as(Minus: Op)
```

`Syntax.char('+').as(Plus)` parses `+` into `Plus`, and when asked to print `Plus` it prints `+`. The printer compares the value it is given with `Plus`, so the second alternative is tried for `Minus`:

```scala mdoc
op.parseString("-")
op.printString(Minus)
op.printString(Plus)
```

### Discarded results need a value to print

A syntax whose result is thrown away, such as whitespace, has no value for the printer. `asPrinted(result, value)` says what to print instead:

```scala mdoc:silent
val spaces: Syntax[String, Char, Char, Unit] =
  Syntax.whitespace.repeat0.asPrinted((), Chunk(' '))

val assignment: Syntax[String, Char, Char, (String, Int)] =
  (Syntax.letter.repeat.string ~ Syntax.char('=').surroundedBy(spaces) ~ number)
```

```scala mdoc
assignment.parseString("x   =   5")
assignment.printString(("x", 5))
```

The parser accepts any amount of whitespace around `=`, and the printer produces exactly one space.

### Subtypes need `widen` or `transformTo`

When a syntax produces a parent type but each alternative produces a subtype, the printer needs to find the alternative that fits a value. `widen[T]` does this with a runtime class check, and `transformTo` takes a partial function and an error for the values it does not cover:

```scala mdoc:silent
sealed trait Shape
final case class Circle(r: Int)         extends Shape
final case class Square(side: Int)      extends Shape

val circle: Syntax[String, Char, Char, Circle] =
  (Syntax.string("circle ", ()) ~> number).transform(Circle.apply, _.r)

val square: Syntax[String, Char, Char, Square] =
  (Syntax.string("square ", ()) ~> number).transform(Square.apply, _.side)

val shape: Syntax[String, Char, Char, Shape] =
  circle.widen[Shape] | square.widen[Shape]
```

```scala mdoc
shape.parseString("square 4")
shape.printString(Circle(2))
shape.printString(Square(7))
```

### Printing can fail

Printing returns an `Either`. A syntax can reject values it cannot print, for example when a `filter` condition is not met or a `transformTo` partial function is not defined:

```scala mdoc:silent
val small: Syntax[String, Char, Char, Int] =
  number.filter[String, Int](_ < 100, "too big")
```

```scala mdoc
small.printString(5)
small.printString(500)
```

## Printers on Their Own

`Syntax` is a pair of a `Parser` and a `Printer`, available as `asParser` and `asPrinter`. A `Printer` can be used independently, for example for pretty printing a data structure that you never parse:

```scala mdoc:silent
val printer: Printer[String, Char, Int] = number.asPrinter
```

```scala mdoc
printer.printString(42)
```

`Printer.printString(str)`, `Printer.char(c)` and `Printer.byValue` let you build printers by hand. Combine a `Parser` and a `Printer` into a syntax with `parser <=> printer`.

## Printing to a Target

`print(value, target)` writes the output elements to a `Target`. A `Target[Output]` is a small interface with `write`, and `capture`/`emit`/`drop` used by the printer internally for backtracking. ZIO Parser ships two implementations:

- `zio.parser.target.ChunkTarget`, which accumulates the output in a `Chunk` and is used by `print`.
- `zio.parser.target.ZStreamTarget`, which pushes the output into a `ZStream`.

Next: [Error handling](errors.md).
