name := "playground-scala"

version := "0.1"

scalaVersion := "3.3.3"

libraryDependencies ++= Seq(
  "org.scalameta"              %% "munit"             % "1.0.1" % Test,
  "org.scalameta"              %% "munit-scalacheck"  % "0.7.29" % Test
)