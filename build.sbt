lazy val versions = new {
  val     config = "1.2.1"
  val      jline = "2.12.1"
  val      log4j = "2.2"
  val    logging = "3.1.0"
  val        rng = "1.3.0"
  val    rxscala = "0.24.0"
  val    rxswing = "0.22.0"
  val      scala = "2.11.6"
  val scalacheck = "1.12.2"
  val    scalafx = "8.0.31-R7"
  val     scalaz = "7.1.1"
  val      scopt = "3.3.0"
  val  shapeless = "0.3"
  val     specs2 = "3.0"
  val      spire = "0.9.1"
  val      swing = "1.0.1"
  val  zForSpecs = "0.3.0"
}

lazy val deps = new {
  import versions._

  val api = List(
    "org.spire-math"              %% "spire"                      % spire      ,
    "org.typelevel"               %% "shapeless-scalaz"           % shapeless
      exclude("org.scalaz", "scalaz-core_2.11")                                )

  val algebra = List(
    "org.scalaz"                  %% "scalaz-core"                % scalaz     )

  val playables = List(
    "org.scala-lang"               % "scala-reflect"              % scala      ,
    "com.typesafe"                 % "config"                     % config     )

  val main = List(
    "com.github.scopt"            %% "scopt"                      % scopt      )

  val gui = List(
    "org.scalaz"                  %% "scalaz-effect"              % scalaz     ,
    "org.scalafx"                 %% "scalafx"                    % scalafx    ,
    "org.scala-lang.modules"      %% "scala-swing"                % swing      ,
    "io.reactivex"                %% "rxscala"                    % rxscala    ,
    "io.reactivex"                 % "rxswing"                    % rxswing    )

  val tui = List(
    "org.scalaz"                  %% "scalaz-effect"              % scalaz     ,
    "jline"                        % "jline"                      % jline      )

  val random = List(
    "org.scalaz"                  %% "scalaz-effect"              % scalaz     ,
    "com.nicta"                   %% "rng"                        % rng
      exclude("org.scalaz", "scalaz-effect_2.11")
      exclude("org.scalaz", "scalaz-core_2.11")                                )

  val log = List(
    "org.scalaz"                  %% "scalaz-effect"              % scalaz     ,
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

lazy val playables = project
  .settings(name := "ff7-playables")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.playables)
  .dependsOn(api)

lazy val core = project
  .configs(RunDebug, RunProfile)
  .settings(debugSettings ++ profileSettings: _*)
  .settings(name := "ff7-core")
  .settings(ff7Settings: _*)
  .dependsOn(playables)

lazy val gui = project.in(file("interpreters") / "gui")
  .settings(name := "ff7-interpreter-gui")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.gui)
  .dependsOn(algebra)

lazy val tui = project.in(file("interpreters") / "tui")
  .settings(name := "ff7-interpreter-tui")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.tui)
  .dependsOn(algebra)

lazy val random = project.in(file("interpreters") / "random")
  .settings(name := "ff7-interpreter-random")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.random)
  .dependsOn(algebra)

lazy val log = project.in(file("interpreters") / "log")
  .settings(name := "ff7-interpreter-log")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.log)
  .dependsOn(algebra)

lazy val main = project
  .configs(RunDebug, RunProfile)
  .settings(debugSettings ++ profileSettings: _*)
  .settings(name := "ff7")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.main)
  .dependsOn(core, gui, tui, random, log)

lazy val tests = project
  .settings(name := "ff7-tests")
  .settings(ff7Settings: _*)
  .settings(libraryDependencies ++= deps.tests)
  .dependsOn(core)

lazy val dist = project
  .settings(
    resourceDirectory <<= baseDirectory { _ / "scripts" },
               target <<= baseDirectory { _ / "app" }
  )

lazy val parent = project.in(file("."))
  .configs(RunDebug, RunProfile)
  .settings(debugSettings ++ profileSettings: _*)
  .settings(name := "ff7-parent")
  .settings(ff7Settings: _*)
  .dependsOn(algebra, api, core, gui, log, main, playables, random, tests, tui)
  .aggregate(algebra, api, core, dist, gui, log, main, playables, random, tests, tui)
  .settings(
    aggregate in dependencySvgView := false,
    aggregate in          assembly := false
  )

// =================================

lazy val buildSettings = List(
        organization := "de.knutwalker",
   projectMaintainer := "Paul Horn",
        scalaVersion := versions.scala,
         buildFatJar := true,
           startYear := Some(2015),
        profilerPath := Path.userHome / "Downloads" /
          "YourKit_Java_Profiler_2015_EAP_build_15028.app" / "Contents" / "Resources" /
          "bin" / "mac" / "libyjpagent.jnilib"
)

lazy val commonSettings = List(
  scalacOptions ++=
    "-encoding" ::  "UTF-8" :: "-language:_" ::
    "-deprecation" :: "-explaintypes" :: "-feature" :: "-unchecked" ::
    "-Xcheckinit" :: "-Xfatal-warnings" :: "-Xfuture" :: "-Xlint" ::
    "-Yclosure-elim" :: "-Ydead-code" :: "-Yno-adapted-args" :: "-Yno-predef" ::
    "-Ywarn-adapted-args" :: "-Ywarn-inaccessible" :: "-Ywarn-nullary-override" :: "-Ywarn-nullary-unit" :: Nil,
  scalacOptions in Test += "-Yrangepos",
  scalacOptions in (Compile, console) ~= (_ filterNot (x ⇒ x == "-Xfatal-warnings" || x.startsWith("-Ywarn"))),
  shellPrompt := { state ⇒
    val name = Project.extract(state).currentRef.project
    (if (name == "parent") "" else name + " ") + "> "
  },
  resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"
)

lazy val runSettings = List(
  initialCommands in      console := """import scalaz._, Scalaz._, ff7._, algebra._, battle._, characters._, monsters._""",
  initialCommands in consoleQuick := """import scalaz._, Scalaz._""",
                      logBuffered := false,
      javaOptions in     RunDebug ++= "-Xdebug" :: "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=5005" :: Nil,
      javaOptions in   RunProfile += s"-agentpath:${profilerPath.value.getAbsolutePath}=tracing",
             fork in          run := true,
     connectInput in          run := true,
             fork in     RunDebug := true,
     connectInput in     RunDebug := true,
             fork in   RunProfile := true,
     connectInput in   RunProfile := true
)

lazy val headerSettings =
  List(headers <<= (projectMaintainer, startYear) { (m, y) ⇒
    val thisYear = java.util.Calendar.getInstance().get(java.util.Calendar.YEAR)
    val years = List(y.getOrElse(thisYear), thisYear).distinct.mkString(" – ")
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

lazy val buildsUberJar = List(
     assemblyJarName in assembly := { if (buildFatJar.value) s"${name.value}" else s"${name.value}_${version.value}.jar" },
  assemblyOutputPath in assembly := (target in dist).value / (assemblyJarName in assembly).value,
           mainClass in assembly := Some("ff7.Main"),
                test in assembly := {},
      assemblyOption in assembly := {
        val starter = IO.readLines((resourceDirectory in dist).value / "starter.sh")
        val opts = (assemblyOption in assembly).value
          .copy(prependShellScript =  Some(starter))
        if (!buildFatJar.value)
          opts.copy(includeScala = false, includeDependency = false)
        else opts
      },
  assemblyMergeStrategy in assembly := {
    case "application.conf" ⇒ MergeStrategy.concat
    case x                  ⇒
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  }
)

lazy val ff7Settings =
  buildSettings ++ commonSettings ++ runSettings ++ headerSettings ++ buildsUberJar

lazy val debugSettings = inConfig(RunDebug)(Defaults.compileSettings)
lazy val profileSettings = inConfig(RunProfile)(Defaults.compileSettings)

lazy val projectMaintainer = SettingKey[String]("Maintainer")
lazy val buildFatJar = SettingKey[Boolean]("true builds a fat jar, false builds only an assembled jar sans dependencies")
lazy val profilerPath = SettingKey[File]("path (directory) to Yourkit profiler agent")

lazy val RunDebug = config("debug") extend Runtime
lazy val RunProfile = config("profile") extend Runtime
