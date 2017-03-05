val dependencies = Seq(
    // test
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)


val commonSettings = Seq(
    scalaVersion := "2.11.8",
    libraryDependencies ++= dependencies
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
