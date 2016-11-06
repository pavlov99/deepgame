lazy val root = (project in file(".")).
  settings(
    name := "deepgame",
    version := "1.0",
    scalaVersion := "2.11.8",
    scalacOptions ++= Seq("-deprecation", "-feature"),  // print deprecation warnings in sbt
    resolvers += Resolver.sonatypeRepo("releases")
  )
