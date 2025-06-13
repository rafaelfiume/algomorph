name := "playground-scala"

version := "0.1"

scalaVersion := "3.7.1"

scalacOptions := scalacOptions.value.filterNot(_ == "-Ykind-projector")

libraryDependencies ++= Seq(
  "org.scalameta"              %% "munit"             % "1.1.1" % Test,
  "org.scalameta"              %% "munit-scalacheck"  % "1.1.0" % Test
)
