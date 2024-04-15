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

[//]: # (TODO: Add example section)
[//]: # (## Example)
