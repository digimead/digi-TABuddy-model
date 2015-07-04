//
// Copyright (c) 2012-2015 Alexey Aksenov ezh@ezh.msk.ru
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// DEVELOPMENT CONFIGURATION

import sbt.osgi.manager._

OSGiManager ++ sbtprotobuf.ProtobufPlugin.protobufSettings //  ++ sbt.scct.ScctPlugin.instrumentSettings

name := "digi-tabuddy-model"

description := "TABuddy data model"

licenses := Seq("The Apache Software License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

organization := "org.digimead"

organizationHomepage := Some(url("http://digimead.org"))

homepage := Some(url("https://github.com/digimead/digi-TABuddy-model"))

version <<= (baseDirectory) { (b) => scala.io.Source.fromFile(b / "version").mkString.trim }

inConfig(OSGiConf)({
  import OSGiKey._
  Seq(
    osgiBndBundleSymbolicName := "org.digimead.tabuddy.model",
    osgiBndBundleCopyright := "Copyright © 2013-2015 Alexey B. Aksenov/Ezh. All rights reserved.",
    osgiBndExportPackage := List("org.digimead.*"),
    osgiBndImportPackage := List("!org.aspectj.*", "*"),
    osgiBndBundleLicense := "http://www.apache.org/licenses/LICENSE-2.0.txt;description=The Apache Software License, Version 2.0"
  )
})

crossScalaVersions := Seq("2.11.6")

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked", "-Xcheckinit", "-feature")

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation")

compileOrder := CompileOrder.JavaThenScala

sources in Compile in doc ~= (_ filter {file => false})

//
// Custom local options
//

version in sbtprotobuf.ProtobufPlugin.protobufConfig := "2.5.0"

resolvers += "digimead-maven" at "http://storage.googleapis.com/maven.repository.digimead.org/"

libraryDependencies ++= Seq(
  "com.google.protobuf" % "protobuf-java" % "2.5.0",
  "org.digimead" %% "digi-lib" % "0.3.1.4",
  "org.digimead" %% "digi-lib-test" % "0.3.2.0" % "test",
  "org.yaml" % "snakeyaml" % "1.15"
)

//
// Testing
//

parallelExecution in Test := false

testGrouping in Test <<= (definedTests in Test, javaOptions in run) map { (tests, javaOptions) ⇒
  tests map { test =>
    new Tests.Group(
      name = test.name,
      tests = Seq(test),
      runPolicy = Tests.SubProcess(ForkOptions(runJVMOptions = Seq.empty[String])))
  }
}

//logLevel := Level.Debug
