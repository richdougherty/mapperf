lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "mapperf",
    scalaVersion := "2.12.4",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    )
  )
