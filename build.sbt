name := "algomorph"

version := "0.1"

scalaVersion := "3.7.1"

scalacOptions := scalacOptions.value.filterNot(_ == "-Ykind-projector")

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

libraryDependencies ++= Seq(
  "org.scalameta"              %% "munit"             % "1.1.1" % Test,
  "org.scalameta"              %% "munit-scalacheck"  % "1.1.0" % Test
)
