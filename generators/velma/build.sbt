organization := "edu.northwestern.eecs"

version := "0.0.5"

name := "velma"

scalaVersion := "2.12.12"

val chiselVersion = "3.5.2"
lazy val chiselSettings = Seq(
  libraryDependencies ++= Seq("edu.berkeley.cs" %% "chisel3" % chiselVersion),
  addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % chiselVersion cross CrossVersion.full)
  )
