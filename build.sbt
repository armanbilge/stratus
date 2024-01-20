ThisBuild / tlBaseVersion := "0.0"
ThisBuild / tlUntaggedAreSnapshots := false

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / startYear := Some(2022)
ThisBuild / developers := List(
  tlGitHubDev("armanbilge", "Arman Bilge"),
)

ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / crossScalaVersions := Seq("3.2.1")
ThisBuild / scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future")
ThisBuild / Test / testOptions += Tests.Argument("+l")

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

val catsVersion = "2.9.0"
val kittensVersion = "3.0.0"
val catsEffectVersion = "3.4.6"
val fs2Version = "3.9.4"
val schrodingerVersion = "0.4-46f23fd"
val spireVersion = "0.18.0"

val munitVersion = "1.0.0-M7"
val munitCEVersion = "2.0.0-M3"
val disciplineMunitVersion = "2.0.0-M3"
val scalaCheckVersion = "1.17.0"
val scalaCheckEffectVersion = "2.0.0-M2"

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "stratus-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "kittens" % kittensVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "co.fs2" %%% "fs2-core" % fs2Version,
      "com.armanbilge" %%% "schrodinger-monte-carlo" % schrodingerVersion,
      "com.armanbilge" %%% "schrodinger-testkit" % schrodingerVersion % Test,
      "org.typelevel" %%% "cats-laws" % catsVersion % Test,
      "org.typelevel" %%% "spire-laws" % spireVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % munitVersion % Test,
      "org.typelevel" %%% "munit-cats-effect" % munitCEVersion % Test,
      "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test,
      "org.typelevel" %%% "scalacheck-effect-munit" % scalaCheckEffectVersion % Test,
    ),
  )
  .jvmSettings(
    Test / fork := true,
  )
