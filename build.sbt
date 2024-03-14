inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.dev/zio-parser/")),
    licenses      := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers    := List(
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
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

// Versions
val scala212 = "2.12.18"
val scala213 = "2.13.12"
val scala3   = "3.3.1"

val zioVersion = "2.0.21"

// Command aliases for convenience and for CI
addCommandAlias("fmt", s"++$scala213; scalafmtSbt; scalafmtAll")
addCommandAlias("check", s"++$scala213; scalafmtSbtCheck; scalafmtCheckAll")

addCommandAlias("testJVM", ";zioParserJVM/test; calibanParser/test")
addCommandAlias("testJS", ";zioParserJS/test")
addCommandAlias("testNative", ";zioParserNative/test")

addCommandAlias("testJVM_212", s"++$scala212 ;zioParserJVM/test; calibanParser/test")
addCommandAlias("testJS_212", s"++$scala212 ;zioParserJS/test")
addCommandAlias("testNative_212", s"++$scala212 ;zioParserNative/test")

addCommandAlias("testJVM_213", s"++$scala213 ;zioParserJVM/test; calibanParser/test; benchmarks/test")
addCommandAlias("testJS_213", s"++$scala213 ;zioParserJS/test")
addCommandAlias("testNative_213", s"++$scala213 ;zioParserNative/test")

addCommandAlias("testJVM_3", s"++$scala3 ;zioParserJVM/test; calibanParser/test")
addCommandAlias("testJS_3", s"++$scala3 ;zioParserJS/test")
addCommandAlias("testNative_3", s"++$scala3 ;zioParserNative/test")

// Common settings for all projects
lazy val commonSettings = Seq(
  crossScalaVersions       := Seq(scala212, scala213, scala3),
  scalaVersion             := scala3,
  Test / parallelExecution := true,
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding",
    "UTF-8",
    "-feature",
    "-unchecked",
    "-language:experimental.macros"
  ) ++ (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 12)) =>
      Seq(
        "-language:higherKinds",
        "-language:existentials",
        "-explaintypes",
        "-Yrangepos",
        "-Xsource:2.13"
      )
    case Some((2, 13)) =>
      Seq(
        "-language:higherKinds",
        "-language:existentials",
        "-explaintypes",
        "-Yrangepos"
      )
    case Some((3, _))  =>
      Seq(
        "-Xfatal-warnings",
        "-Xcheck-macros",
        "-language:implicitConversions",
        "-Xignore-scala2-macros"
      )
    case _             => Seq()
  })
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
  .settings(commonSettings)
  .settings(
    name := "zio-parser",
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          Seq.empty
        case _            =>
          Seq(
            compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full),
            "org.scala-lang" % "scala-reflect"  % scalaVersion.value % Provided,
            "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
          )
      }),
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"          % zioVersion,
      "dev.zio" %%% "zio-streams"  % zioVersion,
      "dev.zio" %%% "zio-test"     % zioVersion % Test,
      "dev.zio" %%% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val zioParserJVM    = zioParser.jvm
lazy val zioParserJS     = zioParser.js
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      ("org.scala-js"               %% "scalajs-test-interface" % scalaJSVersion).cross(CrossVersion.for3Use2_13)
    )
  )
lazy val zioParserNative = zioParser.native
  .settings(
    libraryDependencies ++= Seq("org.scala-native" %%% "test-interface" % nativeVersion)
  )

lazy val calibanParser = project
  .in(file("zio-parser-caliban"))
  .settings(commonSettings)
  .settings(
    name           := "zio-parser-caliban",
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban"      % "2.5.3",
      "dev.zio"               %% "zio"          % zioVersion,
      "dev.zio"               %% "zio-streams"  % zioVersion,
      "dev.zio"               %% "zio-test"     % zioVersion % Test,
      "dev.zio"               %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zioParserJVM)

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    scalaVersion   := scala213,
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse"   % "1.0.0",
      "com.lihaoyi"   %% "fastparse"    % "3.0.2",
      "org.tpolecat"  %% "atto-core"    % "0.9.5",
      "org.parboiled" %% "parboiled"    % "2.5.1",
      "org.http4s"    %% "parsley"      % "1.5.0-M3",
      "org.spartanz"  %% "parserz"      % "0.2.4",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(zioParserJVM)

lazy val docs = project
  .in(file("zio-parser-docs"))
  .settings(
    name                                       := "zio-parser",
    scalaVersion                               := scala213,
    publish / skip                             := true,
    moduleName                                 := "zio-parser-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName                                := "ZIO Parser",
    mainModuleName                             := (zioParserJVM / moduleName).value,
    projectStage                               := ProjectStage.Development,
    docsPublishBranch                          := "master",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioParserJVM)
  )
  .dependsOn(zioParserJVM)
  .enablePlugins(WebsitePlugin)
