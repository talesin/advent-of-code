import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.2"
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.1"
  lazy val scalaTestPlus = "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0"
}
