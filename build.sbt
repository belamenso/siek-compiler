val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Scala 3 Project Template",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      "com.lihaoyi" %% "pprint" % "0.9.0",
    )
  )
