lazy val baseName   = "WritingSimultan"
lazy val baseNameL  = baseName.toLowerCase

lazy val projectVersion = "0.1.0-SNAPSHOT"

lazy val deps = new {
  val main = new {
    val fscape          = "2.29.0-SNAPSHOT"
    val lucre           = "3.14.0-SNAPSHOT"
    val soundProcesses  = "3.31.0-SNAPSHOT"
  }
}

lazy val loggingEnabled = true

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  description        := "A sound installation",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.12.9",
  resolvers          += "Oracle Repository" at "http://download.oracle.com/maven",  // required for sleepycat
  scalacOptions     ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xsource:2.13", "-Xlint:-stars-align,_"),
  scalacOptions     ++= {
    if (loggingEnabled || isSnapshot.value) Nil else Seq("-Xelide-below", "INFO")
  },
  updateOptions      := updateOptions.value.withLatestSnapshots(false)
)

// ---- modules ----

lazy val root = project.withId(baseNameL).in(file("."))
  .settings(commonSettings)
  .settings(
    name := baseName,
    libraryDependencies ++= Seq(
      "de.sciss"  %% "soundprocesses" % deps.main.soundProcesses,
      "de.sciss"  %% "fscape-macros"  % deps.main.fscape,
      "de.sciss"  %% s"lucre-bdb"     % deps.main.lucre
    ),
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in run := true,
    mainClass in (Compile, run) := Some("de.sciss.writingsimultan.Main")
  )

