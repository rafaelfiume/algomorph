name := "algomorph"

version := "0.1"

scalaVersion := "3.8.4"

scalacOptions := scalacOptions.value.filterNot(_ == "-Ykind-projector")

ThisBuild / semanticdbEnabled := true
ThisBuild / semanticdbVersion := scalafixSemanticdb.revision

libraryDependencies ++= Seq(
  "org.scalameta"              %% "munit"             % "1.3.5" % Test,
  "org.scalameta"              %% "munit-scalacheck"  % "1.3.0" % Test
)
