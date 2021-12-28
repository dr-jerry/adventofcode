name := "adventofcode"

version := "0.1"

scalaVersion := "2.13.7"

libraryDependencies ++= Seq (
  "net.ruippeixotog" %% "scala-scraper" % "2.2.1"
  , "com.softwaremill.sttp" %% "core" % "1.6.2"
  , "com.twitter" %% "finagle-http" % "21.9.0"
  , "org.scalactic" %% "scalactic" % "3.2.10"
  , "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)
