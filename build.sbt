enablePlugins(ZioSbtCiPlugin)

inThisBuild(
  List(
    name                   := "ZIO Parser",
    crossScalaVersions -= scala211.value,
    ciEnabledBranches      := Seq("master"),
    javaPlatforms          := Seq("8", "11"),
    ciGroupSimilarTests    := false,
    ciMatrixMaxParallel    := Some(8),
    sbtBuildOptions        := List("-J-XX:+UseG1GC", "-J-Xmx6g", "-J-Xms4g", "-J-Xss16m"),
    developers             := List(
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
    ),
    supportedScalaVersions := Map(
      (zioParserJVM / thisProject).value.id    -> (zioParserJVM / crossScalaVersions).value,
      (zioParserJS / thisProject).value.id     -> (zioParserJS / crossScalaVersions).value,
      (zioParserNative / thisProject).value.id -> (zioParserNative / crossScalaVersions).value,
      (calibanParser / thisProject).value.id   -> (calibanParser / crossScalaVersions).value,
      (benchmarks / thisProject).value.id      -> (benchmarks / crossScalaVersions).value
    ),
    ciCheckAllCodeCompiles := Seq.empty // TODO: remove this line, update ci workflow and fix compilation issues
  )
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
  .settings(
    stdSettings(
      name = "zio-parser",
      packageName = Some("zio.parser"),
      enableCrossProject = true,
      enableKindProjector = true
    ),
    macroDefinitionSettings,
    enableZIO(enableStreaming = true)
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioParserJVM = zioParser.jvm
lazy val zioParserJS  = zioParser.js
  .settings(
    scalaJSUseMainModuleInitializer := true
  )

lazy val zioParserNative = zioParser.native
  .settings(nativeSettings)

lazy val calibanParser = project
  .in(file("zio-parser-caliban"))
  .settings(
    stdSettings("zio-parser-caliban"),
    enableZIO(),
    publish / skip := true,
    libraryDependencies ++= Seq("com.github.ghostdogpr" %% "caliban" % "2.1.0")
  )
  .dependsOn(zioParserJVM)

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    scalaVersion   := scala213.value,
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
  .settings(
    stdSettings("zio-parser"),
    macroDefinitionSettings,
    scalaVersion                               := scala213.value,
    publish / skip                             := true,
    moduleName                                 := "zio-parser-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := (ThisBuild / name).value,
    mainModuleName                             := (zioParserJVM / moduleName).value,
    projectStage                               := ProjectStage.Development,
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioParserJVM)
  )
  .dependsOn(zioParserJVM)
  .enablePlugins(WebsitePlugin)
