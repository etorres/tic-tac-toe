import org.scalafmt.sbt.ScalafmtPlugin.autoImport.scalafmtOnCompile
import sbt.Keys._
import sbt._
import wartremover.Wart
import wartremover.WartRemover.autoImport._

organization := "es.eriktorr"
name := "tick-tack-toe"
version := (version in ThisBuild).value

scalaVersion := "2.13.4"

idePackagePrefix := Some("es.eriktorr")

val catsCoreVersion = "2.2.0"
val catsEffectsVersion = "2.2.0"
val catsScalacheckVersion = "0.3.0"
val kittensVersion = "2.2.1"
val newtypeVersion = "0.4.4"
val refinedVersion = "0.9.19"
val weaverVersion = "0.5.0"

libraryDependencies ++= Seq(
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.11.2" cross CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1" cross CrossVersion.binary),
  "org.typelevel" %% "cats-core" % catsCoreVersion,
  "org.typelevel" %% "cats-effect" % catsEffectsVersion,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.typelevel" %% "kittens" % kittensVersion,
  "io.estatico" %% "newtype" % newtypeVersion,
  "eu.timepit" %% "refined" % refinedVersion,
  "eu.timepit" %% "refined-scalacheck" % refinedVersion % Test,
  "com.disneystreaming" %% "weaver-framework" % weaverVersion % Test,
  "com.disneystreaming" %% "weaver-scalacheck" % weaverVersion % Test
)

dependencyOverrides += "org.typelevel" %% "cats-core" % catsCoreVersion

scalacOptions ++= Seq(
  "-encoding",
  "utf8",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xlint:-byname-implicit",
  "-Ymacro-annotations",
  "-deprecation",
  "-unchecked",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-feature"
)

javacOptions ++= Seq(
  "-g:none",
  "-source",
  "11",
  "-target",
  "11",
  "-encoding",
  "UTF-8"
)

scalafmtOnCompile := true

val warts: Seq[Wart] = Warts.allBut(
  Wart.Any,
  Wart.Nothing,
  Wart.Equals,
  Wart.DefaultArguments,
  Wart.Overloading,
  Wart.ToString,
  Wart.ImplicitParameter,
  Wart.ImplicitConversion // @newtype
)

wartremoverErrors in (Compile, compile) ++= warts
wartremoverErrors in (Test, compile) ++= warts

testFrameworks += new TestFramework("weaver.framework.TestFramework")
