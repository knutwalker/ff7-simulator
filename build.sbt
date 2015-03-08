import com.typesafe.sbt.pgp.PgpKeys._
import sbt._
import sbt.Keys._
import sbtrelease._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import xerial.sbt.Sonatype.SonatypeKeys._

lazy val versions = new {
  val     scalaz = "7.1.1"
  val        rng = "1.3.0"
  val      spire = "0.9.1"
  val   argonaut = "6.1-M5"
  val    monocle = "1.0.1"
  val      jline = "2.12.1"
  val    logging = "3.1.0"
  val      log4j = "2.2"
  val    rxscala = "0.24.0"
  val    rxswing = "0.22.0"
  val  shapeless = "0.3"
  val     specs2 = "3.0"
  val scalacheck = "1.12.2"
  val      swing = "1.0.1"
  val transducer = "0.2.0"
  val  zForSpecs = "0.3.0"
}

lazy val deps = new {
  import versions._

  val api = List(
    "org.spire-math"              %% "spire"                      % spire      ,
    "org.typelevel"               %% "shapeless-scalaz"           % shapeless  ,
    "de.knutwalker"               %% "transducers-scala"          % transducer )

  val algebra = List(
    "org.scalaz"                  %% "scalaz-core"                % scalaz     ,
    "org.scalaz"                  %% "scalaz-effect"              % scalaz     ,
    "com.nicta"                   %% "rng"                        % rng        )

  val console = List(
    "jline"                        % "jline"                      % jline      )

  val gui = List(
    "org.scala-lang.modules"      %% "scala-swing"                % swing      ,
    "io.reactivex"                %% "rxscala"                    % rxscala    ,
    "io.reactivex"                 % "rxswing"                    % rxswing    )

  val core = List(
    "com.typesafe.scala-logging"  %% "scala-logging"              % logging    ,
    "org.apache.logging.log4j"     % "log4j-api"                  % log4j      ,
    "org.apache.logging.log4j"     % "log4j-core"                 % log4j      ,
    "org.apache.logging.log4j"     % "log4j-slf4j-impl"           % log4j      )

  val tests = List(
    "org.specs2"                  %% "specs2-core"                % specs2     ,
    "org.specs2"                  %% "specs2-scalacheck"          % specs2     ,
    "org.scalaz"                  %% "scalaz-scalacheck-binding"  % scalaz     ,
    "org.scalacheck"              %% "scalacheck"                 % scalacheck ,
    "org.typelevel"               %% "shapeless-scalacheck"       % shapeless  ,
    "org.typelevel"               %% "scalaz-specs2"              % zForSpecs  )
    .map(_ % "test")
}

lazy val algebra = project
  .settings(name := "ff7-algebra")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.algebra)

lazy val api = project
  .settings(name := "ff7-api")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.api)
  .dependsOn(algebra)

lazy val equipment = project
  .settings(name := "ff7-equipment")
  .settings(ff7Settings: _*)
  .dependsOn(api)

lazy val formulas = project
  .settings(name := "ff7-formulas")
  .settings(ff7Settings: _*)
  .dependsOn(api)

lazy val characters = project
  .settings(name := "ff7-characters")
  .settings(ff7Settings: _*)
  .dependsOn(equipment)

lazy val console = project
  .settings(name := "ff7-console")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.console)
  .dependsOn(algebra)

lazy val gui = project
  .settings(name := "ff7-gui")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.gui)
  .dependsOn(algebra)

lazy val core = project
  .settings(name := "ff7")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.core)
  .dependsOn(characters, console, equipment, formulas, gui)

lazy val tests = project
  .settings(name := "ff7-tests")
  .settings(ff7Settings: _*)
  .settings(doNotPublish: _*)
  .settings(libraryDependencies ++= deps.tests)
  .dependsOn(core)

lazy val parent = project.in(file("."))
  .settings(name := "ff7-parent")
  .settings(ff7Settings: _*)
  .settings(doNotPublish: _*)
  .dependsOn(algebra, api, characters, console, core, equipment, formulas, gui, tests)
  .aggregate(algebra, api, characters, console, core, equipment, formulas, gui, tests)

// =================================

lazy val javaVersion = SettingKey[String]("Java Version")
lazy val githubUser = SettingKey[String]("Github username")
lazy val githubRepo = SettingKey[String]("Github repository")
lazy val projectMaintainer = SettingKey[String]("Maintainer")

lazy val buildSettings = List(
        organization := "de.knutwalker",
   projectMaintainer := "Paul Horn",
          githubUser := "knutwalker",
          githubRepo := "ff7-simulator",
        scalaVersion := "2.11.6",
         javaVersion := "1.8"
)

lazy val commonSettings = List(
  scalacOptions ++=
    "-encoding" ::  "UTF-8" ::
    "-deprecation" :: "-explaintypes" :: "-feature" :: "-unchecked" ::
    "-language:existentials" :: "-language:higherKinds" :: "-language:implicitConversions" :: "-language:postfixOps" ::
    "-Xcheckinit" :: "-Xfatal-warnings" :: "-Xfuture" :: "-Xlint" ::
    "-Yclosure-elim" :: "-Ydead-code" :: "-Yno-adapted-args" :: "-Yrangepos" ::
    "-Ywarn-adapted-args" :: "-Ywarn-inaccessible" :: "-Ywarn-nullary-override" :: "-Ywarn-nullary-unit" :: Nil,
  javacOptions ++=
    "-source" :: javaVersion.value :: "-target" :: javaVersion.value :: Nil,
  scmInfo <<= (githubUser, githubRepo) { (u, r) ⇒ Some(ScmInfo(
    url(s"https://github.com/$u/$r"),
    s"scm:git:https://github.com/$u/$r.git",
    Some(s"scm:git:ssh://git@github.com:$u/$r.git")
  ))},
  shellPrompt := { state ⇒
    val name = Project.extract(state).currentRef.project
    (if (name == "parent") "" else name + " ") + "> "
  },
  fork in run := true,
  connectInput in run := true,
  logBuffered := false,
  resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
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
