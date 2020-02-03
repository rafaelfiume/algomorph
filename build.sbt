name := "playground-scala"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.scalactic"  %% "scalactic"  % "3.0.8"  % Test,
  "org.scalatest"  %% "scalatest"  % "3.0.8"  % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
)