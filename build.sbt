lazy val root = project
  .in(file("."))
  .settings(
    name := "lambs",
    version := "0.1.0",

    scalaVersion := "0.24.0-RC1",
    
    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-Xfatal-warnings"
    ),

    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.0"   % "test",
      "org.scalacheck"    %% "scalacheck"      % "1.14.1"  % "test",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" % "test"
    ).map(_.withDottyCompat(scalaVersion.value))
  )
