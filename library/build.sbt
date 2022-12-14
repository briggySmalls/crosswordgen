ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "mycrossword",
    idePackagePrefix := Some("mycrossword")
  )
  .enablePlugins(ScalaJSPlugin)

libraryDependencies ++= Seq(
  "org.scalactic" %%% "scalactic" % "3.2.13",
  "org.scalatest" %%% "scalatest" % "3.2.13" % "test",
  "com.lihaoyi" %%% "scalatags" % "0.12.0",
  "org.scalatestplus" %%% "scalacheck-1-16" % "3.2.12.0" % "test"
)
