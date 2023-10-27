name:= "helloworld"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "helloworld"
  )


libraryDependencies += "com.opencsv" % "opencsv" % "5.7.1"