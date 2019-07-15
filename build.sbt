import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "1.0.0"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
    .settings(
        name := "genetic-search",

        libraryDependencies += scalaTest % Test

    )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
