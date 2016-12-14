import sbt._
import Keys._

object FPInScalaBuild extends Build {
  val opts = Project.defaultSettings ++ Seq(
     scalaVersion := "2.11.7",
     resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
   )

  lazy val root = Project("fpinscala", file("."), settings = opts) aggregate exercises

  lazy val exercises = Project("exercises", file("exercises"), settings = opts)
}