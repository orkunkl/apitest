name := "apiTest"

version := "0.1"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-client" % "0.15.16a",
  "org.http4s" %% "http4s-circe" % "0.15.16a",
  "io.circe" %% "circe-parser" % "0.8.0",
  "io.circe" %% "circe-optics" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "io.circe" %% "circe-generic" % "0.8.0"
)
        