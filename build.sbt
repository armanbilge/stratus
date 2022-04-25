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

lazy val catsVersion = "2.7.0"
lazy val kittensVersion = "3.0.0-M4"
lazy val catsEffectVersion = "3.3.11"
lazy val fs2Version = "3.2.7"
lazy val schrodingerVersion = "0.3-cb36e01"

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
      "com.armanbilge" %%% "schrodinger-monte-carlo" % schrodingerVersion
    )
  )
