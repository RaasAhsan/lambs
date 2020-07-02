lazy val root = project
  .in(file("."))
  .settings(
    name := "lambs",
    version := "0.1.0",

    scalaVersion := "0.24.0-RC1",
    
    scalacOptions ++= Seq(
      "-language:implicitConversions"
    ),

    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_0.24" % "3.2.0" % "test"
    )
  )
