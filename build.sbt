lazy val commonSettings = Seq(
  scalaVersion := "2.11.6",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
)

lazy val macros = project.settings(
  commonSettings : _*
).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val sample = project.settings(
  commonSettings : _*
).settings(
  libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.9"
).dependsOn(macros)
