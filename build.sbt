import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "1.1.0"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
    .settings(
        name := "genetic-search",

        libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

    )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
