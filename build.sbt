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

lazy val root = tlCrossRootProject.aggregate(stratus)

lazy val stratus = project
  .in(file("stratus"))
  .settings(
    name := "stratus",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.7.0",
      "org.typelevel" %%% "cats-effect" % "3.3.11",
      "co.fs2" %%% "fs2-core" % "3.2.7"
    )
  )
