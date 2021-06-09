name := "Stratego-SA_Scala3"
organization := "de.htwg.se.Stratego-SA_Scala3"
version := "0.2.0"
scalaVersion := "3.0.0"

lazy val stratego = (project in file("."))
  .settings(
    name := "Stratego-SA_Scala3",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  )
