name := "playground-scala"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "org.scalactic"     %% "scalactic"       % "3.1.1"   % Test,
  "org.scalatest"     %% "scalatest"       % "3.1.4"   % Test,
  "org.scalatestplus" %% "scalacheck-1-14" % "3.1.1.1" % Test
)