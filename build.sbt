name := """hannah"""

organization := "de.ioswarm"

version := "0.1.0"

scalaVersion := "2.12.1"

scalacOptions ++= Seq(
  "-feature",
  "-language:_",
  "-unchecked",
  "-deprecation",
  "-encoding", "utf8"
)

libraryDependencies ++= Seq(
  "io.netty" % "netty-transport" % "4.1.9.Final"
  , "io.netty" % "netty-codec-http" % "4.1.9.Final"
)
