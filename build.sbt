import com.typesafe.sbt.pgp.PgpKeys._
import sbt._
import sbt.Keys._
import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import xerial.sbt.Sonatype.SonatypeKeys._

lazy val githubUser = SettingKey[String]("Github username")
lazy val githubRepo = SettingKey[String]("Github repository")
lazy val projectMaintainer = SettingKey[String]("Maintainer")

lazy val buildSettings = List(
        organization := "de.knutwalker",
   projectMaintainer := "Paul Horn",
          githubUser := "knutwalker",
          githubRepo := "ff7-simulator",
        scalaVersion := "2.11.5",
  crossScalaVersions := "2.11.5" :: "2.10.4" :: Nil
)


lazy val parent = project.in(file("."))
  .settings(name := "ff7-parent")
  .settings(ff7Settings: _*)
  .settings(doNotPublish: _*)
  .dependsOn(core, tests, guide)
  .aggregate(core, tests, guide)

lazy val core = project
  .settings(name := "ff7")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= List(
  "org.scalaz"                 %% "scalaz-core"       % "7.1.1" ,
  "org.scalaz"                 %% "scalaz-effect"     % "7.1.1" ,
  "org.scalaz"                 %% "scalaz-concurrent" % "7.1.1" ,
  "org.typelevel"              %% "shapeless-scalaz"  % "0.3"   ,
  "com.nicta"                  %% "rng"               % "1.3.0" ,
  "org.spire-math"             %% "spire"             % "0.9.1" ,
  "org.typelevel"              %% "shapeless-spire"   % "0.3"
    exclude("org.spire-math", "spire_2.11")                     ,
  "io.argonaut"                %% "argonaut"          % "6.1-M5"
    exclude("com.github.julien-truffaut", "monocle-macro_2.11")
    exclude("com.github.julien-truffaut", "monocle-core_2.11")  ,
  "com.github.julien-truffaut" %% "monocle-core"      % "1.0.1" ,
  "com.github.julien-truffaut" %% "monocle-generic"   % "1.0.1" ,
  "com.github.julien-truffaut" %% "monocle-macro"     % "1.0.1" ,
  "com.typesafe.scala-logging" %% "scala-logging"     % "3.1.0" ,
  "org.apache.logging.log4j"    % "log4j-api"         % "2.2"   ,
  "org.apache.logging.log4j"    % "log4j-core"        % "2.2"   ,
  "org.apache.logging.log4j"    % "log4j-slf4j-impl"  % "2.2"   ))

lazy val guide = project
  .settings(name := "ff7-guide")
  .settings(ff7Settings: _*)
  .settings(doNotPublish: _*)
  .settings(buildInfos: _*)
  .settings(libraryDependencies ++= List(
    "org.specs2" %% "specs2-html" % "2.4.16" % "test"))
  .dependsOn(tests % "test->test")

lazy val tests = project
  .settings(name := "ff7-tests")
  .settings(ff7Settings: _*)
  .settings(doNotPublish: _*)
  .settings(libraryDependencies ++= List(
    "com.github.julien-truffaut" %% "monocle-law"               % "1.0.1" ,
    "org.specs2"                 %% "specs2-core"               % "2.4.16",
    "org.specs2"                 %% "specs2-scalacheck"         % "2.4.16",
    "org.scalacheck"             %% "scalacheck"                % "1.12.2",
    "org.scalaz"                 %% "scalaz-scalacheck-binding" % "7.1.1" ,
    "org.typelevel"              %% "shapeless-scalacheck"      % "0.3"   ,
    "org.typelevel"              %% "scalaz-specs2"             % "0.3.0" )
    .map(_ % "test"))
  .dependsOn(core)

// =================================

lazy val commonSettings = List(
  scalacOptions ++=
    "-encoding" ::  "UTF-8" ::
    "-deprecation" :: "-explaintypes" :: "-feature" :: "-unchecked" ::
    "-language:existentials" :: "-language:higherKinds" :: "-language:implicitConversions" :: "-language:postfixOps" ::
    "-Xcheckinit" :: "-Xfatal-warnings" :: "-Xfuture" :: "-Xlint" ::
    "-Yclosure-elim" :: "-Ydead-code" :: "-Yno-adapted-args" ::
    "-Ywarn-adapted-args" :: "-Ywarn-inaccessible" :: "-Ywarn-nullary-override" :: "-Ywarn-nullary-unit" :: Nil,
  scmInfo <<= (githubUser, githubRepo) { (u, r) ⇒ Some(ScmInfo(
    url(s"https://github.com/$u/$r"),
    s"scm:git:https://github.com/$u/$r.git",
    Some(s"scm:git:ssh://git@github.com:$u/$r.git")
  ))},
  shellPrompt := { state ⇒
    val name = Project.extract(state).currentRef.project
    (if (name == "parent") "" else name + " ") + "> "
  }
)

lazy val publishSettings = List(
                 homepage <<= (githubUser, githubRepo) { (u, r) => Some(url(s"https://github.com/$u/$r")) },
                  licenses := List("Apache License, Verison 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
                 startYear := Some(2015),
         publishMavenStyle := true,
   publishArtifact in Test := false,
      pomIncludeRepository := { _ => false },
  SonatypeKeys.profileName := "knutwalker",
               tagComment <<= (version in ThisBuild) map (v => s"Release version $v"),
            commitMessage <<= (version in ThisBuild) map (v => s"Set version to $v"),
               versionBump := sbtrelease.Version.Bump.Bugfix,

  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra <<= (githubUser, projectMaintainer) { (u, m) ⇒
    <developers>
      <developer>
        <id>${u}</id>
        <name>${m}</name>
        <url>http://knutwalker.de/</url>
      </developer>
    </developers>
  },
  releaseProcess := List[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    setReleaseVersion,
    runClean,
    runTest,
    commitReleaseVersion,
    tagRelease,
    publishSignedArtifacts,
    releaseToCentral,
    setNextVersion,
    commitNextVersion,
    pushChanges,
    publishArtifacts
  )
)

lazy val publishSignedArtifacts = publishArtifacts.copy(
  action = { st: State =>
    val extracted = Project.extract(st)
    val ref = extracted.get(Keys.thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  },
  enableCrossBuild = true
)

lazy val releaseToCentral = ReleaseStep(
  action = { st: State =>
    val extracted = Project.extract(st)
    val ref = extracted.get(Keys.thisProjectRef)
    extracted.runAggregated(sonatypeReleaseAll in Global in ref, st)
  },
  enableCrossBuild = true
)

lazy val doNotPublish = List(
          publish := (),
     publishLocal := (),
  publishArtifact := false
)

lazy val headerSettings =
  List(headers <<= (projectMaintainer, startYear) { (m, y) ⇒
    val years = List(y.get, java.util.Calendar.getInstance().get(java.util.Calendar.YEAR)).distinct.mkString(" – ")
    val license =
      s"""|/*
          | * Copyright $years $m
          | *
          | * Licensed under the Apache License, Version 2.0 (the "License");
          | * you may not use this file except in compliance with the License.
          | * You may obtain a copy of the License at
          | *
          | *     http://www.apache.org/licenses/LICENSE-2.0
          | *
          | * Unless required by applicable law or agreed to in writing, software
          | * distributed under the License is distributed on an "AS IS" BASIS,
          | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
          | * See the License for the specific language governing permissions and
          | * limitations under the License.
          | */
          |
          |""".stripMargin
    Map("java" -> (HeaderPattern.cStyleBlockComment, license),
        "scala" -> (HeaderPattern.cStyleBlockComment, license))
  }) ++
  inConfig(Compile)(compileInputs.in(compile) <<= compileInputs.in(compile).dependsOn(createHeaders.in(compile))) ++
  inConfig(Test)(compileInputs.in(compile) <<= compileInputs.in(compile).dependsOn(createHeaders.in(compile)))

lazy val buildInfos = buildInfoSettings ++ List(
  sourceGenerators in Test <+= buildInfo,
  buildInfoPackage := "buildinfo",
     buildInfoKeys := List[BuildInfoKey](
       organization,
       name in core,
            version,
       scalaVersion,
  BuildInfoKey("dependencies" → (libraryDependencies in core).value.distinct))
)

lazy val buildsUberJar = List(
        assemblyJarName in assembly := s"${name.value}_${scalaBinaryVersion.value}-${version.value}.jar",
     assemblyOutputPath in assembly := (baseDirectory in parent).value / (assemblyJarName in assembly).value,
         assemblyOption in assembly ~= { _.copy(includeScala = false) }
)

lazy val ff7Settings =
  buildSettings ++ commonSettings ++ publishSettings ++ releaseSettings ++ headerSettings
