name := "transformers"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.1" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"

testFrameworks += new TestFramework("utest.runner.Framework")
