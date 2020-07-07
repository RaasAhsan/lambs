lazy val root = project
  .in(file("."))
  .settings(
    name := "lambs",
    version := "0.1.0",

    scalaVersion := "2.13.2",
    
    scalacOptions ++= Seq(
      "-language:implicitConversions",
      "-Xfatal-warnings"
    ),

    libraryDependencies ++= Seq(
      "org.scalatest"     %% "scalatest"       % "3.2.0"   % "test",
      "org.scalacheck"    %% "scalacheck"      % "1.14.3"  % "test",
      "org.scalatestplus" %% "scalacheck-1-14" % "3.2.0.0" % "test"
    )
  )
