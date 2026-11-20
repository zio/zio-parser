---
id: index
title: "Introduction to ZIO Parser"
sidebar_label: "Introduction"
---

Library for constructing parsers and pretty printers based on invertible syntax descriptions

@PROJECT_BADGES@

## Introduction

ZIO Parser describes a grammar once, as a `Syntax`, and uses that description in both directions: it parses text into values and prints values back to text. Because one value drives both, a parser and its pretty printer cannot drift apart.

The library gives you small building blocks (characters, strings, regexes), operators to combine them, and error values that say where and why a parse failed.

## Installation

Add ZIO Parser to your `build.sbt`. It is published for Scala 2.12, 2.13 and 3.

On the JVM, use `%%`:

```scala
// ZIO Parser: syntaxes, parsers, printers and regexes (JVM)
libraryDependencies += "dev.zio" %% "zio-parser" % "@VERSION@"
```

On Scala.js and Scala Native, use `%%%`:

```scala
// ZIO Parser: syntaxes, parsers, printers and regexes (Scala.js and Scala Native)
libraryDependencies += "dev.zio" %%% "zio-parser" % "@VERSION@"
```

## Where to Go Next

- [Getting Started](getting-started.md): parse and print your first value.
- [Printing and Invertible Syntaxes](printing.md): the idea behind one syntax that both parses and prints.
- [Syntax](syntax.md), [Regex](regex.md), [Error Handling](errors.md), [Parser Reference](reference-parser.md) and [Printer Reference](reference-printer.md): the API.
- [Tutorial: Parse and Print a Config Language](tutorial.md), [Example: a JSON Syntax](example-json.md) and [Example: a GraphQL Parser](example-caliban.md): step-by-step learning.
- [How-to Guides](how-to-whitespace-and-comments.md): whitespace and comments, nested data, readable error messages, streaming output and performance.
- [Resources](resources.md): talks and further reading.
