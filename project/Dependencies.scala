import sbt.Keys._
import sbt._

object Dependencies {
  object Versions {
    val specs2 = "3.6.6"
  }

  val shapeless = Seq("com.chuusai" %% "shapeless" % "2.3.0")

  val cats = Seq("org.typelevel" %% "cats" % "0.6.0")

  val kittens = Seq("org.typelevel" %% "kittens" % "1.0.0-M3")

  val test = Seq(
    "org.specs2" %% "specs2-core" % Versions.specs2 % "test",
    "org.specs2" %% "specs2-mock" % Versions.specs2 % "test"
  )

  val commonSettings = Seq(
    scalaVersion in ThisBuild := "2.11.8",
    resolvers ++= Seq(
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots"),
      "bintray/non" at "http://dl.bintray.com/non/maven",
      Resolver.bintrayRepo("scalaz", "releases")
    ),
    libraryDependencies ++= kittens ++ cats,

    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1"),
    addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.1.0" cross CrossVersion.full)
  )

  val testSettings = commonSettings ++ Seq(
    libraryDependencies ++= test
  )

  val settings = commonSettings

}
