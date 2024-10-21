ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "kamin"
  )
libraryDependencies += "org.jline" % "jline" % "3.23.0"
libraryDependencies ++= Seq(
  "org.scalatestplus" %% "mockito-5-10" % "3.2.18.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)
