lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin)
  .settings(
    name := "mapperf",
    scalaVersion := "2.12.4",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
  )
