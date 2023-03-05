val zioSbtVersion = "0.3.10+60-2af21ed8-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.timushev.sbt" % "sbt-updates"     % "0.6.4")
addSbtPlugin("dev.zio"          % "zio-sbt-website" % "0.3.4")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.6"

resolvers += Resolver.sonatypeRepo("public")
