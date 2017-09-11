name := "apiTest"

version := "0.1"

scalaVersion := "2.12.3"

scalafmtOnCompile in ThisBuild := true
scalafmtTestOnCompile := true // current project

wartremoverWarnings ++= Warts.all // or Warts.unsafe

libraryDependencies ++= Seq(
  "org.http4s"             %% "http4s-blaze-client" % "0.15.16a",
  "org.http4s"             %% "http4s-circe"        % "0.15.16a",
  "io.circe"               %% "circe-parser"        % "0.8.0",
  "org.scalatest"          %% "scalatest"           % "3.0.1" % "test",
  "com.github.nscala-time" %% "nscala-time"         % "2.16.0",
)
