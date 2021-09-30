val scala212 = "2.12.15"
val scala213 = "2.13.6"
val scala3   = "3.0.2"

val zioVersion = "1.0.12"

val scalacOptions212 = Seq(
  "-deprecation",
  "-Ypartial-unification"
)

val scalacOptions213 = Seq(
  "-deprecation"
)

val scalacOptions3 = Seq(
  "-deprecation",
  "-Ykind-projector",
  "-explain"
)

ThisBuild / scalaVersion := scala213
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "dev.zio"
ThisBuild / organizationName := "zio"
ThisBuild / scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
  case Some((2, 12)) => scalacOptions212
  case Some((2, 13)) => scalacOptions213
  case Some((3, _))  => scalacOptions3
  case _             => Nil
})

lazy val root = (project in file("."))
  .aggregate(
    zioParser,
    calibanParser,
  )
  .settings(
    crossScalaVersions := Nil,
    publish / skip := false
  )


lazy val zioParser = (project in file("zio-parser"))
  .settings(
    name := "zio-parser",
    crossScalaVersions := Seq(scala212, scala213, scala3),
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq.empty
        case _            =>
          Seq(
            compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
          )
      }),
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-streams"  % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    scalaVersion := scala213,
    crossScalaVersions := Seq(scala213),
    scalacOptions := scalacOptions213,
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse"   % "0.3.4",
      "com.lihaoyi"   %% "fastparse"    % "2.3.2",
      "org.tpolecat"  %% "atto-core"    % "0.9.5",
      "org.parboiled" %% "parboiled"    % "2.3.0",
      "org.http4s"    %% "parsley"      % "1.5.0-M3",
      "org.spartanz"  %% "parserz"      % "0.2.4",
      "dev.zio"       %% "zio-test"     % zioVersion % Test,
      "dev.zio"       %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
  .enablePlugins(JmhPlugin)
  .dependsOn(zioParser)

lazy val calibanParser = (project in file("zio-parser-caliban"))
  .dependsOn(zioParser)
  .settings(
    name := "zio-parser-caliban",
    crossScalaVersions := Seq(scala212, scala213, scala3),
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban"      % "1.1.0",
      "dev.zio"               %% "zio"          % zioVersion,
      "dev.zio"               %% "zio-streams"  % zioVersion,
      "dev.zio"               %% "zio-test"     % zioVersion % Test,
      "dev.zio"               %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
