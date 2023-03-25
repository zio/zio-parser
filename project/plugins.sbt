val zioSbtVersion = "0.3.10+101-92de7e6b-SNAPSHOT"

addSbtPlugin("dev.zio" % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio" % "zio-sbt-ci"        % zioSbtVersion)

addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.6.4")

resolvers ++= Resolver.sonatypeOssRepos("public")
