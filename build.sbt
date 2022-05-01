ThisBuild / tlBaseVersion := "0.0"
ThisBuild / tlUntaggedAreSnapshots := false

ThisBuild / organization := "com.armanbilge"
ThisBuild / organizationName := "Arman Bilge"
ThisBuild / developers := List(
  tlGitHubDev("armanbilge", "Arman Bilge")
)

ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / crossScalaVersions := Seq("3.1.2")
ThisBuild / scalacOptions ++= Seq("-new-syntax", "-indent", "-source:future")

ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))
ThisBuild / tlJdkRelease := Some(8)

val catsVersion = "2.7.0"
val kittensVersion = "3.0.0-M4"
val catsEffectVersion = "3.3.11"
val fs2Version = "3.2.7"
val schrodingerVersion = "0.3-b10e24f"
val spireVersion = "0.18.0-M3"

val munitVersion = "0.7.29"
val disciplineMunitVersion = "1.0.9"
val scalaCheckVersion = "1.16.0"

lazy val root = tlCrossRootProject.aggregate(core)

lazy val core = project
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
      "com.armanbilge" %%% "schrodinger-stats" % schrodingerVersion % Test,
      "org.typelevel" %%% "cats-laws" % catsVersion % Test,
      "org.typelevel" %%% "spire-laws" % spireVersion % Test,
      "org.scalameta" %%% "munit-scalacheck" % munitVersion % Test,
      "org.typelevel" %%% "discipline-munit" % disciplineMunitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalaCheckVersion % Test
    ),
    Test / fork := true
  )
