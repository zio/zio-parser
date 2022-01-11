---
id: overview_index
title: "Summary"
---

## Getting started

Start by adding `zio-parser` as a dependency to your project:
  
```scala mdoc:passthrough
    println(s"""```scala""")
    if (zio.parser.BuildInfo.isSnapshot) {
        println(s"""resolvers += Resolver.sonatypeRepo("snapshots")""")
    }
    println(s"""libraryDependencies += "dev.zio" %% "zio-parser" % "${zio.parser.BuildInfo.version}"""")
    println(s"""```""")
```

