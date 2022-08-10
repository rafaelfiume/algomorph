name := "playground-scala"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "org.scalameta"              %% "munit"             % "0.7.29" % Test,
  "org.scalameta"              %% "munit-scalacheck"  % "0.7.29" % Test
)