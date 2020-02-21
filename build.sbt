val commonSettings = Seq(
  scalaVersion := "2.13.1"
)

lazy val root = (project in file("."))
  .aggregate(exercises, answers)
  .settings(commonSettings)
  .settings(
    name := "fpinscala"
  )

lazy val exercises = (project in file("exercises"))
  .settings(commonSettings)
  .settings(
    name := "exercises"
  )

lazy val answers = (project in file("answers"))
  .settings(commonSettings)
  .settings(
    name := "answers"
  )

lazy val tests = (project in file("tests"))
  .settings(commonSettings)
  .settings(
    name := "tests"
  )
  .dependsOn(exercises)
