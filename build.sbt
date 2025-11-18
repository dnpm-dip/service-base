
import scala.util.Properties.envOrElse

name := "service-base"
ThisBuild / organization := "de.dnpm.dip"
ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := envOrElse("VERSION","1.1.2")

val ownerRepo  = envOrElse("REPOSITORY","dnpm-dip/service-base").split("/")
ThisBuild / githubOwner      := ownerRepo(0)
ThisBuild / githubRepository := ownerRepo(1)

//-----------------------------------------------------------------------------
// PROJECT
//-----------------------------------------------------------------------------

lazy val root = project.in(file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
    "org.scalatest"  %% "scalatest"  % "3.2.18" % Test,
    "de.dnpm.dip"    %% "core"       % "1.1.4",
    "de.ekut.tbi"    %% "validators" % "1.0.0",
    "de.ekut.tbi"    %% "generators" % "1.0.0" % Test
   )
 )


//-----------------------------------------------------------------------------
// SETTINGS
//-----------------------------------------------------------------------------

lazy val settings = commonSettings


// Compiler options from: https://alexn.org/blog/2020/05/26/scala-fatal-warnings/
lazy val compilerOptions = Seq(
  // Feature options
  "-encoding", "utf-8",
  "-explaintypes",
  "-feature",
  "-language:existentials",
  "-language:experimental.macros",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ymacro-annotations",

  // Warnings as errors!
  "-Xfatal-warnings",

  // Linting options
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:doc-detached",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wvalue-discard",
)


lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Local Maven Repository" at "file://" + Path.userHome.absolutePath + "/.m2/repository",
    Resolver.githubPackages("dnpm-dip"),
    Resolver.githubPackages("KohlbacherLab"),
    Resolver.sonatypeCentralSnapshots
  )
)
