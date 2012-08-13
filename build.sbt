name := "GUV Analyzer"

version := "1.0a"

scalaVersion := "2.9.2"

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

retrieveManaged := true

mainClass in (Compile, run) := Some("com.jamesrthompson.Controllers.Launch")

JFX.mainClass := "com.jamesrthompson.Controllers.Launch"

JFX.sdkDir := "/Library/Java/JavaVirtualMachines/1.7.0.jdk/Contents/Home"

JFX.javaOnly := false