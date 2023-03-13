val zioSbtVersion = "0.3.10+78-eb4ed5f9-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")

resolvers ++= Resolver.sonatypeOssRepos("public")
