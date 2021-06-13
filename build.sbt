name := "Stratego-SA_Scala3"
organization := "de.htwg.se.Stratego-SA_Scala3"
version := "0.2.0"
scalaVersion := "3.0.0"

lazy val stratego = (project in file("/de.htwg.se.stratego"))
  .settings(
    name := "Stratego-SA_Scala3",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
    libraryDependencies += "com.google.inject" % "guice" % "5.0.1",
    libraryDependencies += "org.scala-lang.modules" % "scala-xml_2.12" % "1.0.6",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.6",
    libraryDependencies += "ch.epfl.scala" %% "scala-debug-adpater" % "1.0.0"
  )
