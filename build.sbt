import BuildHelper._

inThisBuild(
  List(
    organization  := "dev.zio",
    homepage      := Some(url("https://zio.github.io/zio-parser/")),
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

val zioVersion = "2.0.0-RC5"

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
  .settings(stdSettings("zio-parser"))
  .settings(crossProjectSettings)
  .settings(dottySettings)
  .settings(buildInfoSettings("zio.parser"))
  .settings(macroDefinitionSettings)
  .settings(
    libraryDependencies ++=
      (CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) => Seq.empty
        case _            =>
          Seq(
            compilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
          )
      }),
    libraryDependencies ++= Seq(
      "dev.zio"               %% "zio"          % zioVersion,
      "dev.zio"               %% "zio-streams"  % zioVersion,
      "com.github.ghostdogpr" %% "caliban"      % "1.4.0",
      "dev.zio"               %% "zio-test"     % zioVersion % Test,
      "dev.zio"               %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .enablePlugins(BuildInfoPlugin)

lazy val zioParserJVM = zioParser.jvm
lazy val zioParserJS  = zioParser.js
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioParserNative = zioParser.native
  .settings(nativeSettings)

lazy val calibanParser = project
  .in(file("zio-parser-caliban"))
  .settings(stdSettings("zio-parser-caliban"))
  .settings(dottySettings)
  .dependsOn(zioParserJVM)
  .settings(
    publish / skip := true,
    libraryDependencies ++= Seq(
      "com.github.ghostdogpr" %% "caliban"      % "1.4.0",
      "dev.zio"               %% "zio"          % zioVersion,
      "dev.zio"               %% "zio-streams"  % zioVersion,
      "dev.zio"               %% "zio-test"     % zioVersion % Test,
      "dev.zio"               %% "zio-test-sbt" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

lazy val benchmarks = (project in file("benchmarks"))
  .settings(
    scalaVersion   := Scala213,
    publish / skip := true,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse"   % "0.3.6",
      "com.lihaoyi"   %% "fastparse"    % "2.3.3",
      "org.tpolecat"  %% "atto-core"    % "0.9.5",
      "org.parboiled" %% "parboiled"    % "2.3.0",
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
  .settings(stdSettings("zio-parser"))
  .settings(
    scalaVersion                               := Scala213,
    publish / skip                             := true,
    moduleName                                 := "zio-parser-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioParserJVM),
    ScalaUnidoc / unidoc / target              := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite                       := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages                   := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(zioParserJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
