name := "keepintouch"

version := "1.0.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "1.12.3" % "test")

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                  "releases"  at "http://oss.sonatype.org/content/repositories/releases")
