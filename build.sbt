name := "intellijson"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.json4s" % "json4s-native_2.11" % "3.3.0"

scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")