---
id: index
title: "Introduction to ZIO Parser"
sidebar_label: "ZIO Parser"
---

Library for constructing parsers and pretty printers based on invertible syntax descriptions

@PROJECT_BADGES@

## Introduction

[![Zymposium - ZIO Parser](https://i.ytimg.com/vi/DEPpL9LBiyA/maxresdefault.jpg)](https://www.youtube.com/watch?v=DEPpL9LBiyA)

## Installation

Start by adding `zio-parser` as a dependency to your project:
  
```scala
libraryDependencies += "dev.zio" %% "zio-parser" % "@VERSION@"
```

## Getting Started

```scala mdoc
import zio.parser.*
```

Declare your parsing syntax:

```scala mdoc:silent
val digitSyntax: Syntax[String, Char, Char, Char] = Syntax.digit
```

Parse your string:

```scala mdoc
val result: Either[StringParserError[String], Char] = digitSyntax.parseString("1")
```

Pretty print the parsing errors:

```scala mdoc
println(digitSyntax.parseString("Hello").left.map(_.pretty).merge)
```

## Where to Go Next

- [Tutorial: Parse and Print a Config Language](tutorial.md): build a parser and printer step by step. Start here if you are new to parser combinators.
- [How-To Guides](how-to-whitespace-and-comments.md): whitespace and comments, nested data, readable error messages, streaming output and performance.
- [Printing and Invertible Syntaxes](printing.md): the idea behind one syntax that both parses and prints.
- [Syntax reference](syntax.md): primitives, sequencing, alternatives, repetition and mapping.
- [Regex reference](regex.md): matching whole tokens efficiently.
- [Parser reference](reference-parser.md) and [Printer reference](reference-printer.md): the core types and their combinators.
- [Error Handling](errors.md): error types, naming syntaxes and pretty printing errors.
- [Example: a JSON Syntax](example-json.md) and [Example: a GraphQL Parser](example-caliban.md): complete grammars for real formats.
