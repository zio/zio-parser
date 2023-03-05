import BuildHelper._

enablePlugins(ZioSbtCiPlugin)

inThisBuild(
  List(
    name              := "ZIO Parser",
    ciEnabledBranches := Seq("master"),
    developers        := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      ),
      Developer(
        "vigoo",
        "Daniel Vigovszky",
        "daniel.vigovszky@gmail.com",
        url("https://github.com/vigoo")
      )
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";zioParserJVM/test; calibanParser/test"
)
addCommandAlias(
  "testJS",
  ";zioParserJS/test"
)
addCommandAlias(
  "testNative",
  ";zioParserNative/test"
)

lazy val root = (project in file("."))
  .aggregate(
    zioParserJVM,
    zioParserJS,
    zioParserNative,
    calibanParser,
    docs
  )
  .settings(
    crossScalaVersions := Nil,
    publish / skip     := true
  )

lazy val zioParser = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-parser"))
  .settings(stdSettings(name = "zio-parser", packageName = Some("zio.parser"), enableCrossProject = true))
  .settings(macroDefinitionSettings)
  .settings(
    enableZIO(enableStreaming = true),
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq.empty
        case _            =>
          Seq(
            compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
          )
      })
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioParserJVM = zioParser.jvm
lazy val zioParserJS  = zioParser.js
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion.value % Test)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioParserNative = zioParser.native
  .settings(nativeSettings)

lazy val calibanParser = project
  .in(file("zio-parser-caliban"))
  .settings(stdSettings("zio-parser-caliban"))
  .settings(enableZIO())
  .dependsOn(zioParserJVM)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban" % "2.0.2"
    )
  )

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    scalaVersion   := Scala213,
    publish / skip := true,
    enableZIO(),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.6",
      "com.lihaoyi"   %% "fastparse"  % "2.3.3",
      "org.tpolecat"  %% "atto-core"  % "0.9.5",
      "org.parboiled" %% "parboiled"  % "2.3.0",
      "org.http4s"    %% "parsley"    % "1.5.0-M3",
      "org.spartanz"  %% "parserz"    % "0.2.4"
    )
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(zioParserJVM)

lazy val docs = project
  .in(file("zio-parser-docs"))
  .settings(stdSettings("zio-parser"))
  .settings(macroDefinitionSettings)
  .settings(
    scalaVersion                               := Scala213,
    publish / skip                             := true,
    moduleName                                 := "zio-parser-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := "ZIO Parser",
    mainModuleName                             := (zioParserJVM / moduleName).value,
    projectStage                               := ProjectStage.Development,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioParserJVM)
  )
  .dependsOn(zioParserJVM)
  .enablePlugins(WebsitePlugin)
