import sbt._
import Keys._

name := "scala-di-showdown"

lazy val commonSettings = Seq(
  organization := "com.softwaremill",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.11.8"
)

lazy val scalaDiShowdown = (project in file("."))
  .settings(commonSettings)
  .aggregate(core)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "0.9.0"
    )
  )
