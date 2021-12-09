val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "connectx",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10" % "test",
      "org.scalatest" %% "scalatest-wordspec" % "3.2.10" % "test"
    )
  )
