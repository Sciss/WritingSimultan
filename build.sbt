lazy val baseName   = "WritingSimultan"
lazy val baseNameL  = baseName.toLowerCase

lazy val projectVersion = "0.3.0-SNAPSHOT"

lazy val deps = new {
  val main = new {
    val fscape          = "2.33.0"
    val lucre           = "3.16.1"
    val lucreSwing      = "1.20.0"
    val soundProcesses  = "3.33.0"
    val span            = "1.4.3"
  }
}

lazy val loggingEnabled = true

lazy val commonSettings = Seq(
  version            := projectVersion,
  organization       := "de.sciss",
  homepage           := Some(url(s"https://git.iem.at/sciss/$baseName")),
  description        := "A sound installation",
  licenses           := Seq("AGPL v3+" -> url("http://www.gnu.org/licenses/agpl-3.0.txt")),
  scalaVersion       := "2.13.1",
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
      "de.sciss"  %% "span"           % deps.main.span,
      "de.sciss"  %% "lucre-expr"     % deps.main.lucre,
      "de.sciss"  %% "lucre-bdb"      % deps.main.lucre,
      "de.sciss"  %% "lucre-swing"    % deps.main.lucreSwing,
    ),
    scalacOptions += "-Yrangepos",  // this is needed to extract source code
    fork in run := true,
    mainClass in (Compile, run) := Some("de.sciss.writingsimultan.Main")
  )

