/*
 *  Builder.scala
 *  Writing (simultan)
 *
 *  Copyright (c) 2019 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.writingsimultan

import de.sciss.file._
import de.sciss.fscape.GE
import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.lucre.MacroImplicits._
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{artifact, stm}
import de.sciss.synth
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import de.sciss.synth.proc
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.Workspace
import de.sciss.writingsimultan.BuilderUtil._

object Builder {
  val DEFAULT_VERSION = 1

//  protected def any2stringadd: Any = ()

  def apply[S <: Sys[S]](/*audioBaseDir: File*/)(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val audioBaseDir  = userHome
    val dbDir         = audioBaseDir / "db"
    val phDir         = audioBaseDir / "ph"
    val tmpDir        = audioBaseDir / "tmp"
    val dbFile0       = dbDir / "db0.aif"
    val phFile0       = phDir / "ph0.aif"
    val tmpFile0      = tmpDir / "rec.irc"
//    tx.afterCommit {
//      dbDir .mkdirs()
//      phDir .mkdirs()
//      tmpDir.mkdirs()
//    }

    val r             = workspace.root
    val fAux          = mkFolder(r, "aux")
    val loc           = mkObj[S, artifact.ArtifactLocation](fAux, "base", DEFAULT_VERSION) {
      artifact.ArtifactLocation.newVar[S](audioBaseDir)
    }
    val artTmp = mkObj[S, artifact.Artifact](fAux, "rec", DEFAULT_VERSION) {
      artifact.Artifact[S](loc, tmpFile0)
    }

    val dbCount         = mkObj[S, IntObj   ](fAux, "db-count"        , DEFAULT_VERSION)(IntObj.newVar[S](0))
    val phCount         = mkObj[S, IntObj   ](fAux, "ph-count"        , DEFAULT_VERSION)(IntObj.newVar[S](0))

    val cueDb = mkObj[S, proc.AudioCue.Obj](fAux, "database", DEFAULT_VERSION) {
//      val artDb = artifact.Artifact[S](loc, dbFile0)
      val spec0 = synth.io.AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, numChannels = 1, sampleRate = 48000.0)
//      tx.afterCommit {
//        // create empty file
//        if (!dbFile0.exists()) {
//          val af = synth.io.AudioFile.openWrite(dbFile0, spec0)
//          af.close()
//        }
//      }
//      proc.AudioCue.Obj[S](artDb, spec0, 0L, 1.0)
      // N.B.: Obj.Bridge can only do in-place update with Expr.Var!
      proc.AudioCue.Obj.newVar[S](
        proc.AudioCue(
          dbFile0 /*artDb*/, spec0, 0L, 1.0
        )
      )
    }

    val cuePh = mkObj[S, proc.AudioCue.Obj](fAux, "phrase", DEFAULT_VERSION) {
      val spec0 = synth.io.AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, numChannels = 1, sampleRate = 48000.0)
//      tx.afterCommit {
//        // create empty file
//        if (!phFile0.exists()) {
//          val af = synth.io.AudioFile.openWrite(phFile0, spec0)
//          af.close()
//        }
//      }
      // N.B.: Obj.Bridge can only do in-place update with Expr.Var!
      proc.AudioCue.Obj.newVar[S](
        proc.AudioCue(
          phFile0 /*artPh*/, spec0, 0L, 1.0
        )
      )
    }

    val pQueryRadioRec  = mkObj[S, proc.Proc](fAux, "query-radio-rec" , DEFAULT_VERSION)(mkProcQueryRadioRec [S]())
    val pAppendDb       = mkObj[S, FScape   ](fAux, "database-append" , DEFAULT_VERSION)(mkFScAppendDb       [S]())
    val pFillDb         = mkObj[S, proc.Control](fAux, "fill-database", DEFAULT_VERSION) {
      mkCtlFillDb(pQueryRadioRec = pQueryRadioRec, pAppendDb = pAppendDb, artTmp = artTmp, cueDb = cueDb,
        dbCount = dbCount)
    }

    val fscOvrSelect = mkObj[S, FScape](fAux, "overwrite-select-fsc" , DEFAULT_VERSION)(mkFScOverwriteSelect[S]())
    val pOvrSelect = mkObj[S, proc.Control](fAux, "overwrite-select", DEFAULT_VERSION) {
      mkCtlSelectOverwrite(
        fscOvrSelect = fscOvrSelect,
        cuePh = cuePh,
//        phCount = phCount,
      )
    }

    val fscMatchSelect = mkObj[S, FScape](fAux, "match-select" , DEFAULT_VERSION)(
      mkFScMatchSelect[S](cueDb = cueDb, cuePh = cuePh)
    )

    val fscOvrPerform = mkObj[S, FScape](fAux, "overwrite-perform" , DEFAULT_VERSION)(
      mkFScOvrPerform[S]()
    )

    val pProcPlay = mkObj[S, proc.Proc](fAux, "play-phrase-proc", DEFAULT_VERSION)(mkProcPlayPhrase[S]())

    val pCtlPlay = mkObj[S, proc.Control](fAux, "play-phrase", DEFAULT_VERSION) {
      mkCtlPlayPhrase(procPlay = pProcPlay, cuePh = cuePh)
    }

    val pRender = mkObj[S, proc.Control ](fAux, "render", DEFAULT_VERSION)(
      mkCtlRender(
        pFillDb       = pFillDb,
        pOvrSelect    = pOvrSelect,
        pMatchSelect  = fscMatchSelect,
        pOvrPerform   = fscOvrPerform,
        cueDb         = cueDb,
        cuePh         = cuePh,
        dbCount       = dbCount,
        phCount       = phCount,
      )
    )

    val pRenderLoop = mkObj[S, proc.Control](fAux, "render-loop", DEFAULT_VERSION)(mkCtlLoopRun(pRender ))
    val pPlayLoop   = mkObj[S, proc.Control](fAux, "play-loop"  , DEFAULT_VERSION)(mkCtlLoopRun(pCtlPlay))

    //    mkObj[S, proc.Control ](r, "main"     , DEFAULT_VERSION)(mkCtlMain())

    mkObj[S, proc.Markdown](r, "read-me", DEFAULT_VERSION)(mkMarkdownReadme())
    mkObj[S, proc.Markdown](r, "license", DEFAULT_VERSION)(mkMarkdownLicense())

    val fscInit = mkObj[S, FScape](fAux, "init-fsc", DEFAULT_VERSION)(mkFScInitialize())
    mkObj[S, proc.Widget  ](r, "initialize", DEFAULT_VERSION)(mkWgtInitialize(
      fscInit,
      locBase = loc,
      cueDb   = cueDb,
      cuePh   = cuePh,
      artTmp  = artTmp,
      dbCount = dbCount,
      phCount = phCount,
    ))

    mkObj[S, proc.Widget  ](r, "control", DEFAULT_VERSION)(mkWgtControl(
      pRenderLoop = pRenderLoop,
      pPlayLoop   = pPlayLoop,
      cueDb   = cueDb,
      cuePh   = cuePh,
      dbCount = dbCount,
      phCount = phCount,
    ))
  }

  protected def longWrapper: Any = ()

  def mkProcQueryRadioRec[S <: Sys[S]]()(implicit tx: S#Tx): proc.Proc[S] = {
    val p = proc.Proc[S]()

    import de.sciss.synth.proc.graph.Ops.stringToControl
    import de.sciss.synth.proc.graph._
    import de.sciss.synth.ugen.{DiskOut => _, _}

    p.setGraph {
      // test setup; let's just record from the line input
      val in  = PhysicalIn.ar
      val dur = "dur".ir
      dur.poll(0, "query-radio-rec dur")
      DiskOut.ar("out", in)
      // XXX TODO --- there should be a DoneSelf
      StopSelf(Done.kr(Line.ar(dur = dur)))
    }

    p
  }

  def mkFScAppendDb[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()

    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      def mkInApp() = AudioFileIn("in-app")
      def mkInDb()  = AudioFileIn("in-db")

      val inDb        = AudioFileIn("in-db")
      val inApp0      = mkInApp()
      val captFrames  = "capt-frames".attr
      val SR          = 48000.0

      // Warning: there are still issues with using
      // the same UGens in an If-Then-Els  and
      // its corresponding predicate. The cheesy
      // work around is to make sure we have independent
      // expansion, thus using `def` here instead of `val`.
      def numFrames   = captFrames min mkInApp().numFrames
      def len0        = mkInDb().numFrames
      def fdLen: GE   = len0 min (numFrames min (SR * 0.1).toInt) // .toInt

      // adjust to target for 72 phons
      val loudWin   = SR.toInt
      val inAppSl   = Sliding(inApp0, size = loudWin, step = loudWin/2)
      val loud      = Loudness(in = inAppSl, sampleRate = SR, size = loudWin)
      //      val loudAvg = {
      //        val sum = RunningSum(loud)
      //        val num = Length(sum)
      //        sum.last / num
      //      }
      val loudMax = {
        RunningMax(loud).last
      }
      val loudCorr = {
        val tgt   = 72.0
        val dif   = tgt - loudMax
        ((dif.abs.pow(0.85) * dif.signum) * 1.28).dbAmp.min(24)
      }
      val inApp   = (mkInApp() * loudCorr).clip2(1.0)

      val cat: GE = If (fdLen /*.elastic(4)*/ sig_== 0) Then {
        //  DC(0).take(1).poll(0, "branch-1")
        inDb ++ inApp
      } Else {
        //  DC(0).take(1).poll(0, "branch-2")
        val preLen  = len0 - fdLen
        val pre     = inDb.take(preLen)
        val cross0  = inDb.drop(preLen)  * Line(1, 0, fdLen).sqrt
        val cross1  = inApp.take(fdLen ) * Line(0, 1, fdLen).sqrt
        val cross   = (cross0 + cross1).clip2(1.0)
        val post    = inApp.drop(fdLen)
        pre ++ cross ++ post
      }
      // AudioFileSpec(AIFF, Int16, numChannels = 1, sampleRate = SR)
      AudioFileOut("out-db" /*file = db1F*/, sampleRate = SR, in = cat)
    }

    f
  }

  def mkCtlLoopRun[S <: Sys[S]](
                                 peer: stm.Obj[S],
                               )
                               (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("play"   , peer)

    import de.sciss.lucre.expr.graph._

    c.setGraph {
      val r = Runner("play")

      LoadBang()  ---> r.run
      r.done      ---> r.run
    }

    c
  }

  def mkMarkdownReadme[S <: Sys[S]]()(implicit tx: S#Tx): proc.Markdown[S] = {
    val text =
      """# Read Me
        |
        |This is version 0.1 - the first, in early stage - of the sound algorithms
        |of _Writing (Simultan)_, an installation piece that reconfigures the two
        |previous instances _Writing Machine_ (2011) and _wr\_t\_ng m\_ch\_n\__
        |(2017), within the context of the artistic research project
        |_Algorithms that Matter_.
        |
        |## Background
        |
        |The basic principle of the piece(s) is to initiate a possibly endless
        |writing and rewriting process, that puts the eigenmotion of that process
        |to the front, and the signification of the "raw" or "input" sound material
        |to the background.
        |
        |The basic algorithm is always similar: There is a sound "database", a
        |reservoir of sound material, that is always kept "filled" till a certain
        |level. A process iteratively evolves a "sound phrase", a sound object
        |of a few dozen seconds, by identifying a portion in the current object
        |that should be overwritten, then searching the database for an
        |acoustically similar fragment, cutting it out from the database and
        |pasting it over the identified location. At the same time, small movements
        |are allowed that make the sound phrase slowly expand and contract in
        |duration.
        |
        |The display of the installation is through a circular field of petri-dishes
        |which contain piezo speakers, arranged in a certain number of channels, and
        |thus the sound gesture may move around the circle as it evolves.
        |
        |In the __first instance__ (_Writing Machine_), I used a television sound
        |signal from a news channel. It was using 72 petri-dishes in three concentric
        |circles, each circle using graphite power of a different granularity and
        |appearence. The sound installation was run from a single computer, and
        |using eight or nine channels.
        |
        |In the __second instance__ (_wr\_t\_ng m\_ch\_n\__), I used a local radio
        |(FM) station signal. The petri-dish arrangement was identical, but this time
        |I experimented with a "distributed sound memory", using nine Raspberry Pis,
        |each representing two channels or memories which were evolved independent
        |from one another (so you would identify a sound phrase, each time it was
        |returning to the same sector). Also, here the computers and the antenna were
        |visible, and I selected only one type of graphite and combined it with the
        |bodies of dead bees.
        |
        |The physical arrangement of the __third instance__ is still not decided
        |(as of this writing), although I think I want to replace the "disc" like
        |shape of the petri-dish placement with a form that places them along the
        |walls of the exhibition site, e.g. on side boards. The new title
        |_Writing (Simultan)_ also highlights a new interest in the
        |coming-together of otherwise independently operating processes. How this
        |is translated into the algorithms, will be elaborated in future versions
        |of this workspace.
        |
        |## This Workspace
        |
        |This _Mellite_ workspace contains an ongoing experiment to recreate (and
        |develop) the piece with the software now being fully implemented within
        |_Mellite_. The first instance was using an early version of
        |_Sound Processes_, the second instance was using a recent version of
        |_Sound Processes_, but the algorithm's logic was still implemented in terms
        |of plain Scala code, using the software API and writing the code in a
        |regular code editor. This new version attempts to understand, how working
        |in a live programming environment such as _Mellite_ changes my approach of
        |developing the piece, as well as understanding and testing new abstractions
        |for the formulation of the algorithms (notably the so-called the `Control`
        |object with its `Ex`, `Act`, `Trig` abstractions).
        |
        |The current version is an early skeleton in which I have translated (and
        |still simplified) the algorithm of _wr\_t\_ng m\_ch\_n\__, i.e. I took the
        |plain Scala code of the control logic, and translated it into `Control`
        |objects. This still does not contain the movements of the parameters over
        |time, nor does it represent the networked multi-channel interaction, it
        |just implements a single channel of evolution.
        |
        |To run the experiment, you have to configure the _Mellite_ preferences
        |so that in the audio section, there is at least one input and one output
        |channel. Then make sure to boot the audio server in the main window,
        |before proceeding (you can choose 'automatic boot' in the preferences).
        |As I'm not yet decided on the sound input signal, the workspace just uses
        |the first input channel which is probably your microphone signal, or you
        |could wire it up using the Jack Audio Connection Kit (Linux, mac) to
        |receive a sound signal from any other real-time source.
        |
        |The workspace itself contains a small 'control' user interface that you
        |can open. The main search and replace algorithm is activated with the
        |'Render Loop' checkbox. Once you click that, you should see messages
        |appearing in the post window. Initially your database and phrase files
        |will be empty, so it will take a while, until the database has been
        |filled to its given maximum (currently: three minutes). The 'Play Loop'
        |then simply starts playing back the current sound phrase in a loop, so it
        |advances as the phrase is updated. It currently begins with a very short
        |sound, which then evolves over time to a target duration of thirty seconds.
        |This will change in the future.
        |
        |Should you ever want to start over, you can stop both loops, and reset the
        |database and phrase counters using the 'Reset State' button.
        |
        |__Note:__ The algorithm currently does not delete old database and phrase
        |files, so they may start taking significant space (several gigabytes) on
        |your harddrive. After the experiments, you may wish to delete these files
        |on your harddrive.
        |
        |Please feel free to look into all the partial code objects inside the
        |'aux' folder, and if you want to learn more about them, get in touch.
        |""".stripMargin
    val m = proc.Markdown.newVar(proc.Markdown.newConst(text))
    m.attr.put("edit-mode", BooleanObj.newVar(false))
    m
  }

  def mkMarkdownLicense[S <: Sys[S]]()(implicit tx: S#Tx): proc.Markdown[S] = {
    val text =
      """# Workspace License: CC BY-SA 4.0
        |
        |The workspace and its contents (sound code etc.) was authored by
        |Hanns Holger Rutz, and is made available under the
        |Creative Commons Attribution-ShareAlike 4.0 International license
        |( __CC&nbsp;BY-SA&nbsp;4.0__ ).
        |
        |See [creativecommons.org/licenses/by-sa/4.0](https://creativecommons.org/licenses/by-sa/4.0/)
        |
        |----
        |
        |Attribution-ShareAlike 4.0 International (CC BY-SA 4.0)
        |This is a human-readable summary of (and not a substitute for) the license. Disclaimer.
        |You are free to:
        |
        | - Share — copy and redistribute the material in any medium or format
        | - Adapt — remix, transform, and build upon the material
        |    for any purpose, even commercially.
        |""".stripMargin
    val m = proc.Markdown.newVar(proc.Markdown.newConst(text))
    m.attr.put("edit-mode", BooleanObj.newVar(false))
    m
  }

  def mkWgtControl[S <: Sys[S]](
                                 pRenderLoop: stm.Obj[S],
                                 pPlayLoop  : stm.Obj[S],
                                 cueDb      : proc.AudioCue.Obj[S],
                                 cuePh      : proc.AudioCue.Obj[S],
                                 dbCount    : IntObj[S],
                                 phCount    : IntObj[S],
                             )
                               (implicit tx: S#Tx): proc.Widget[S] = {
    val w = proc.Widget[S]()
    w.attr.put("render-loop", pRenderLoop)
    w.attr.put("play-loop"  , pPlayLoop)
    w.attr.put("database"   , cueDb)
    w.attr.put("phrase"     , cuePh)
    w.attr.put("db-count"   , dbCount)
    w.attr.put("ph-count"   , phCount)
    w.attr.put("edit-mode" , BooleanObj.newVar(false))

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    import de.sciss.synth.proc.ExImport._

    w.setGraph {
      val dbCueIn   = "database".attr[AudioCue](AudioCue.Empty())
      val phCueIn   = "phrase"  .attr[AudioCue](AudioCue.Empty())
      val dbCount   = "db-count".attr(0)
      val phCount   = "ph-count".attr(0)
      val bReset    = Bang()
      val bInfo     = Bang()

      val dbFileIn  = dbCueIn.artifact
      val phFileIn  = phCueIn.artifact
      val dbFileOut = dbFileIn.replaceName("db0.aif")
      val phFileOut = phFileIn.replaceName("ph0.aif")

      bReset ---> Act(
        dbCount.set(0),
        phCount.set(0),
        // dbCueIn.set(AudioCue(dbFileOut, AudioFileSpec.Empty())),
        dbCueIn.set(AudioCue(dbFileOut, AudioFileSpec.Read(dbFileOut).getOrElse(AudioFileSpec.Empty()))),
        // phCueIn.set(AudioCue(phFileOut, AudioFileSpec.Empty())),
        phCueIn.set(AudioCue(phFileOut, AudioFileSpec.Read(phFileOut).getOrElse(AudioFileSpec.Empty()))),
      )

      bInfo ---> Act(
        PrintLn("db-count = " ++ dbCount.toStr),
        PrintLn("db-cue   = " ++ dbCueIn.toStr),
        PrintLn("ph-count = " ++ phCount.toStr),
        PrintLn("ph-cue   = " ++ phCueIn.toStr),
      )

      def mkLooper(key: String): Component = {
        val ggLp  = CheckBox()
        val rLp   = Runner(key)
        val vLp   = ggLp.selected()
        vLp   .toTrig ---> rLp.run
        (!vLp).toTrig ---> rLp.stop
        ggLp
      }

      val ggRenderLoop = mkLooper("render-loop")
      val ggPlayLoop   = mkLooper("play-loop")

      val p = GridPanel(
        Label("Render Loop:") , ggRenderLoop,
        Label("Play Loop:")   , ggPlayLoop,
        Label("Reset State:") , bReset,
        Label("Info:")        , bInfo,
      )

      p.border          = Border.Empty(4)
      p.columns         = 2
      p.compactColumns  = true
      p
    }
    w
  }

  def mkFScOverwriteSelect[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      val spaceDur = "space-dur".attr

      def mkIn() = AudioFileIn("in")

      val fileIn    = mkIn()
      //      val specIn    = AudioFile.readSpec(fileIn)
      //      import specIn.{numChannels, numFrames, sampleRate}
      import fileIn.{numFrames, sampleRate}
      // require(numChannels == 2) // left channel is sound signal, right channel is 'withering'

      val in          = fileIn /*mkIn()*/ out 0
      val inWither    = mkIn() out 1    // separate UGen so we don't run into trouble wrt to buffering

      // XXX TODO --- enabling this prevents some hanging. but why?
      // if (Main.showLog) {
      in.poll(0, "ovr-fsc")
      // }

      val fftSize           = 1024 // 2048
      val stepDiv           = 4
      val numMel            = 42
      val numCoef           = 21
      val sideDur           = 0.25
      val minFreq           = 100.0
      val maxFreq           = 14000.0
      val witherTgt         = 0.0012 / 30   // a threshold of 0.0012 in 30 iterations
      val WitheringConstant = 0.0078125   // = 1.0/128

      val stepSize    = fftSize / stepDiv
      val sideFrames  = (sampleRate * sideDur ).floor // toInt
      val spaceFrames = (sampleRate * spaceDur).floor // toInt
      val spaceLen    = spaceFrames / stepSize
      val sideLen     = (sideFrames / stepSize).max(1) // 24
      val covSize     = numCoef * sideLen
      val numSteps    = numFrames / stepSize
      val numCov      = (numSteps - (2 * sideLen))      .max(0)
      val numCov1     = (numSteps - sideLen - spaceLen) .max(0)

      //numSteps.poll(0, "numSteps")
      //numCov1.poll(0, "numCov1")

      val lap         = Sliding (in       , fftSize, stepSize) * GenWindow(fftSize, GenWindow.Hann)
      val fft         = Real1FFT(lap, fftSize, mode = 2)
      val mag         = fft.complex.mag
      val mel         = MelFilter(mag, fftSize/2, bands = numMel,
        minFreq = minFreq, maxFreq = maxFreq, sampleRate = sampleRate)
      val mfcc        = DCT_II(mel.log, numMel, numCoef, zero = 1 /* 0 */)

      // reconstruction of what strugatzki's 'segmentation' is doing (first step)
      val mfccSlid    = Sliding(mfcc, size = covSize, step = numCoef)
      val mfccSlidT   = mfccSlid.drop(covSize)
      val el          = BufferMemory(mfccSlid, size = covSize)
      val cov         = Pearson(el, mfccSlidT, covSize)

      val covNeg      = -cov + (1: GE)  // N.B. not `1 - cov` because binary-op-ugen stops when first input stops
      val wither0     = ResizeWindow(inWither, size = stepSize, start = 0, stop = -(stepSize - 1))
      val wither      = wither0 * (witherTgt / WitheringConstant)

      val key         = (covNeg + wither).take(numCov1)
      //    Length(BufferDisk(covNeg)).poll(0, "covNeg.length")
      //    Length(BufferDisk(wither)).poll(0, "wither.length")
      //Length(BufferDisk(key   )).poll(0, "key   .length")

      val covMin0     = DetectLocalMax(key, size = spaceLen)
      val covMin      = covMin0.take(numCov)  // XXX TODO --- bug in DetectLocalMax?

      val keysEl      = key.elastic()
      val values      = Frames(keysEl) - 1
      val keysG       = FilterSeq(keysEl, covMin)
      val valuesG     = FilterSeq(values, covMin)

      //    RunningMax(keysG).last.poll(0, "MAX SCHNUCK")

      val top         = PriorityQueue(keysG, valuesG, size = 1)    // lowest covariances mapped to frames
      val startF      = (top * stepSize) :+ 0L
      val valuesGE    = BufferMemory(valuesG, numCov1)
      val stopF       = (valuesG.dropWhile(valuesGE <= top) ++ numCov /* numSteps */)/*.take(1) */ * stepSize

      // def ValueOut(name: String, value: GE): Unit = ()
      //
      // ValueOut("start", startF)
      // ValueOut("stop" , stopF )

      MkLong("start", startF)
      MkLong("stop" , stopF )
    }
    f
  }
  def mkCtlPlayPhrase[S <: Sys[S]](procPlay: proc.Proc[S],
                                    cuePh: proc.AudioCue.Obj[S],
                                  )
                                  (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("play"   , procPlay)
    c.attr.put("phrase" , cuePh)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.synth.proc.ExImport._

    /*
  def play()(implicit tx: Txn): Double = {
    import TxnLike.peer

    val ph0 = phFile()
    if (ph0.numFrames > 4800) {
      val fadeIn  = 0.1f
      import numbers.Implicits._
      val fadeOut = random.nextFloat().linLin(0, 1, 0.1f, 0.5f)
      val start   = 0L
      val stop    = ph0.numFrames
      client.scene.play(ph0, ch = channel, start = start, stop = stop, fadeIn = fadeIn, fadeOut = fadeOut)
      ph0.numFrames / SR
    } else {
      0.0
    }
  }
       */

    c.setGraph {
      val r         = ThisRunner()
      val rPlay     = Runner("play")
      val phCue     = "phrase"  .attr[AudioCue](AudioCue.Empty())
      //val artPh     = Artifact("play:in"  )
      val aIn       = "play:in".attr[AudioCue]
      // val phFile    = phCue.artifact
      val fadeIn    = 0.1
      val fadeOut   = 0.25 // XXX TODO: random.nextFloat().linLin(0, 1, 0.1f, 0.5f)
      val aFdIn     = "play:fade-in"  .attr[Double]
      val aFdOut    = "play:fade-out" .attr[Double]
      val aDur      = "play:dur"      .attr[Double]
      val phDur     = phCue.numFrames / phCue.sampleRate
      val actPlay = Act(
        //  artPh .set(phFile),
        aIn   .set(phCue),
        aFdIn .set(fadeIn),    // XXX TODO --- runWith not yet supported
        aFdOut.set(fadeOut),
        aDur  .set(phDur),
        rPlay.run,
      )

      val init      = LoadBang()
      init ---> actPlay

      rPlay.stoppedOrDone ---> Act(
        PrintLn("phrase play done."),
        r.done,
      )

      rPlay.failed ---> Act(
        PrintLn("phrase play failed."),
        r.fail(rPlay.messages.mkString("\n"))
      )
    }

    c
  }

  def mkProcPlayPhrase[S <: Sys[S]]()(implicit tx: S#Tx): proc.Proc[S] = {
    val p = proc.Proc[S]()

    import de.sciss.synth.Curve
    import de.sciss.synth.proc.graph.Ops.stringToControl
    import de.sciss.synth.proc.graph._
    import de.sciss.synth.ugen.{DiskOut => _, VDiskIn => _, _}

    p.setGraph {
      val bus     = "bus"     .ir(0.0)
      val dur     = "dur"     .ir
      val fdIn    = "fade-in" .ir
      val fdOut   = "fade-out".ir
      val disk    = VDiskIn.ar("in", loop = 0) // numChannels = 2, buf = buf, speed = BufRateScale.ir(buf), loop = 0)
      val chan    = disk out 0 // if (numChannels == 1) disk else Select.ar(bus, disk)
      val hpf     = HPF.ar(chan, 80.0)
      val env     = Env.linen(attack = fdIn, sustain = dur - (fdIn + fdOut), release = fdOut, curve = Curve.sine)
      val amp     = "amp".kr(1.0)
      val eg      = EnvGen.ar(env, levelScale = amp /* , doneAction = freeSelf */)
      val done    = Done.kr(eg)
      val sig     = hpf * eg
      Out.ar(bus, sig)
      StopSelf(done)
    }

    p
  }

  def mkCtlRender[S <: Sys[S]](pFillDb     : stm.Obj[S],
                               pOvrSelect  : stm.Obj[S],
                               pMatchSelect: stm.Obj[S],
                               pOvrPerform : stm.Obj[S],
                               cueDb: proc.AudioCue.Obj[S],
                               cuePh: proc.AudioCue.Obj[S],
                               dbCount     : IntObj[S],
                               phCount     : IntObj[S],
                               )
                              (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("database-fill"    , pFillDb)
    c.attr.put("overwrite-select" , pOvrSelect)
    c.attr.put("match-select"     , pMatchSelect)
    c.attr.put("overwrite-perform", pOvrPerform)
    c.attr.put("database"         , cueDb)
    c.attr.put("phrase"           , cuePh)
    c.attr.put("db-count"         , dbCount)
    c.attr.put("ph-count"         , phCount)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.synth.proc.ExImport._

    /*
        for {
      _     <- dbFill()
      instr <- atomic { implicit tx => phSelectOverwrite  ()            }
      mat   <- atomic { implicit tx => dbFindMatch        (instr)       }
      _     <- atomic { implicit tx => performOverwrite   (instr, mat)  }
    } yield {

      log("iterate() - done")
      ()
    }

     */

    c.setGraph {
      val r           = ThisRunner()
      val rDbFill     = Runner("database-fill")
      val rOvrSelect  = Runner("overwrite-select")
      val rMatchSelect= Runner("match-select")
      val rOvrPerform = Runner("overwrite-perform")

      val resOvrSpan  = Var[Span]()
      val resOvrNewLen= Var[Long]()
      val resInsPos   = Var[Long]()

      val init        = LoadBang()
      init          ---> rDbFill.run
      rDbFill.done  ---> rOvrSelect.runWith(
        "span"        -> resOvrSpan,
        "new-length"  -> resOvrNewLen,
      )
      rOvrSelect.done ---> Act(
        PrintLn(
          "instr: " ++ resOvrSpan.toStr ++
          ", newLength = " ++ resOvrNewLen.toStr
        ),
        rMatchSelect.runWith(
          "start"       -> resOvrSpan.start,
          "stop"        -> resOvrSpan.stop,
          "new-length"  -> resOvrNewLen,
          "result"      -> resInsPos,
        )
      )

      val dbCueIn         = "database".attr[AudioCue](AudioCue.Empty())
      val phCueIn         = "phrase"  .attr[AudioCue](AudioCue.Empty())
      val dbFileIn        = dbCueIn.artifact
      val phFileIn        = phCueIn.artifact
      val ovrPerfDbIn     = Artifact("overwrite-perform:in-db"  )
      val ovrPerfDbOut    = Artifact("overwrite-perform:out-db" )
      val ovrPerfPhIn     = Artifact("overwrite-perform:in-ph"  )
      val ovrPerfPhOut    = Artifact("overwrite-perform:out-ph" )
      val dbCount         = "db-count".attr(0)
      val phCount         = "ph-count".attr(0)

      val doneMatch       = rMatchSelect.done

      val dbCount1        = dbCount.latch(doneMatch) + (1: Ex[Int])
      val phCount1        = phCount.latch(doneMatch) + (1: Ex[Int])
      val dbFileOut       = dbFileIn.replaceName("db" ++ dbCount1.toStr ++ ".aif")
      val phFileOut       = phFileIn.replaceName("ph" ++ phCount1.toStr ++ ".aif")

      doneMatch ---> Act(
        PrintLn("match start: " ++ resInsPos.toStr),
        ovrPerfDbIn .set(dbFileIn ),   // in-db
        ovrPerfDbOut.set(dbFileOut),   // out-db
        ovrPerfPhIn .set(phFileIn ),   // in-ph
        ovrPerfPhOut.set(phFileOut),   // out-ph
        rOvrPerform.runWith(
          "start"       -> resOvrSpan.start,
          "stop"        -> resOvrSpan.stop,
          "new-length"  -> resOvrNewLen,
          "match-pos"   -> resInsPos,
        )
      )

      val specDbOut = AudioFileSpec.read(dbFileOut)
        .getOrElse(AudioFileSpec.Empty())
      val dbCueOut  = AudioCue(dbFileOut, specDbOut)
      val specPhOut = AudioFileSpec.read(phFileOut)
        .getOrElse(AudioFileSpec.Empty())
      val phCueOut  = AudioCue(phFileOut, specPhOut)

      rOvrPerform.done ---> Act(
        PrintLn("iteration done"),
        dbCount.set(dbCount1),
        dbCueIn.set(dbCueOut),
        PrintLn(dbCueIn.toStr),
        phCount.set(phCount1),
        phCueIn.set(phCueOut),
        PrintLn(phCueIn.toStr),
        r.done
      )

      Seq(
        rDbFill      -> "database-fill",
        rOvrSelect   -> "overwrite-select",
        rMatchSelect -> "match-select",
        rOvrPerform  -> "perform-overwrite",
      ).foreach { case (runner, key) =>
        runner.failed ---> PrintLn(runner.messages.mkString(s"FAILURE: $key :\n", "\n", ""))
      }
    }

    c
  }

  def mkFScOvrPerform[S <: Sys[S]](
                                   )
                                   (implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()

    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      // XXX TODO: `def`s here because of deadlock between
      // If predicate and branch use
      def instrSpanStart  = "start"     .attr
      def instrSpanStop   = "stop"      .attr
      def instrNewLen     = "new-length".attr
      val matchPos        = "match-pos" .attr

      val WitheringConstant = 0.0078125   // = 1.0/128
      val SR                = 48000.0

      val phPos       = instrSpanStart
      val dbPos       = matchPos
      // XXX TODO: `def`s here: see above
      def spliceLen   = instrNewLen // min(dbSpan.length, instrNewLen)
      def insSpanLen  = instrSpanStop - instrSpanStart
      def mkInsFdLen  = ((insSpanLen/2) min (spliceLen/2) min (SR * 0.1).toInt).floor

      val inDb        = AudioFileIn("in-db") // db0.f, numChannels = 1)
      val inDbW       = inDb  * (Seq[GE](1.0, 0.0): GE)
      val inPh0       = AudioFileIn("in-ph") // ph0.f, numChannels = 2)
      val inPh        = inPh0 + (Seq[GE](0.0, WitheringConstant): GE) // withering

      val insPre      = inPh.take(phPos)
      val insPost     = inPh.drop(instrSpanStop)
      val insMid      = inDbW.drop(dbPos).take(spliceLen)

      val insCat      = If (mkInsFdLen sig_== 0) Then {
        insPre ++ insMid ++ insPost

      } Else {
        val insFdLen = mkInsFdLen
        // we could use .sqrt for left channel and linear for right channel;
        // for simplicity, just use linear for both.
        // NOTE: we do not fade in/out the withering channel, because that
        // would build up stuff at the phrase beginning and end!
        val preOut  = inPh.drop(phPos).take(insFdLen) * Line(Seq[GE](1.0, 0.0), 0.0, insFdLen) // .sqrt
        val midIn   = insMid          .take(insFdLen) * Line(Seq[GE](0.0, 1.0), 1.0, insFdLen) // .sqrt
        val cross0  = preOut + midIn
        val cross1  = insMid.drop(insFdLen).take(spliceLen - 2*insFdLen)
        val midOut  = insMid.drop(spliceLen     - insFdLen)                * Line(1.0, Seq[GE](0.0, 1.0), insFdLen) // .sqrt
        val postIn  = inPh  .drop(instrSpanStop - insFdLen).take(insFdLen) * Line(0.0, Seq[GE](1.0, 0.0), insFdLen) // .sqrt
        val cross2  = midOut + postIn
        val cross   = cross0 ++ cross1 ++ cross2

        insPre ++ cross ++ insPost
      }
      AudioFileOut("out-ph" /*file = ph1F*/, sampleRate = SR, in = insCat.clip2(1.0))

      val remPre  = inDb.take(dbPos)
      val remPost = inDb.drop(dbPos + spliceLen)

      val remCat  = remPre ++ remPost
      AudioFileOut("out-db" /*file = db1F*/, sampleRate = SR, in = remCat)
    }

    f
  }

  def mkFScMatchSelect[S <: Sys[S]](
                                     cueDb: proc.AudioCue.Obj[S],
                                     cuePh: proc.AudioCue.Obj[S],
                                   )
                                   (implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()
    f.attr.put("database" , cueDb)
    f.attr.put("phrase"   , cuePh)

    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      val instrSpanStart  = "start"     .attr
      val instrSpanStop   = "stop"      .attr
      val instrNewLen0    = "new-length".attr
      val dbLen0          = AudioFileIn("database").numFrames
      val phraseLen       = AudioFileIn("phrase"  ).numFrames

      // require(dbSpec.numChannels == 1)
      // require(phSpec.numChannels == 2) // left channel is sound signal, right channel is 'withering'

      def mkDbIn()    = AudioFileIn("database") // (dbFile, numChannels = dbSpec.numChannels)
      def mkPhIn()    = {
        val in = AudioFileIn("phrase") // (phFile, numChannels = phSpec.numChannels)
        // there is a bloody bug in fscape audio-file-in with the second channel dangling.
        // this seems to fix it. XXX TODO --- is this still the case?
        // Length(in).poll(0, "length-ph-in")
        in out 0
      }

      val SR          = 48000.0
      val fftSize     = 1024 // 2048
      val stepDiv     = 4
      val sideDur     = 0.25
      val numMel      = 42
      val numCoef     = 21
      val minFreq     = 100.0
      val maxFreq     = 14000.0
      // limit, because the Pi is too slow to run the
      // entire 3 minutes in one rotation
      val maxDbDur    = 72.0 // 42.0
      val maxDbLen    = (maxDbDur * SR).toLong

      val stepSize    = fftSize / stepDiv
      val sideFrames  = (SR * sideDur ).toInt
      val sideLen     = math.max(1, sideFrames / stepSize)
      val covSize     = numCoef * sideLen

      //    - punch-in : start = max(0, min(phrase-length, instr.span.start) - sideFrames); stop = min(phrase-length, start + sideFrames)
      //    - punch-out: start = max(0, min(phrase-length, instr.span.stop )); stop = min(phrase-length, start + sideFrames)
      //    - from this we calculate the offset between the two taps into the db: delay-length = punch-out.start - punch-in.start
      //    - from that we calculate the run length: db.length - sideFrames - delay-length; if that's <= 0, abort here
      //      - from that we calculate the number of window steps (/repetitions)

      val instrNewLen     = instrNewLen0 min dbLen0

      // val phraseLen     = phSpec.numFrames
      val punchInStart  = ((phraseLen min instrSpanStart) - sideFrames).max(0L)
      val punchInStop   = phraseLen min (punchInStart + sideFrames)

      val punchOutStart = (phraseLen min instrSpanStop).max(0L)
      val punchOutStop  = phraseLen min (punchOutStart + sideFrames)

      case class Span(start: GE, stop: GE) {
        def length: GE = stop - start
      }

      val punchLen      = (punchInStop - punchInStart) min (punchOutStop - punchOutStart)
      val punchIn       = Span(punchInStart , punchInStart  + punchLen)
      val punchOut      = Span(punchOutStart, punchOutStart + punchLen)

      // val dbLen         = min(maxDbLen, dbSpec.numFrames)
      val dbLen         = (maxDbLen: GE) min dbLen0
      val dbDlyFrames   = (instrNewLen - sideFrames).max(0L)
      val runFrames     = (dbLen - sideFrames - dbDlyFrames).max(0L)
      val runSteps      = (runFrames / stepSize).max(1)

      val dbSpanIn      = Span(0L, runFrames)
      val dbSpanOut     = Span(dbDlyFrames, dbDlyFrames + runFrames)

      def mkMatrix(in: GE): GE = {
        val lap         = Sliding(in, fftSize, stepSize) * GenWindow(fftSize, GenWindow.Hann)
        val fft         = Real1FFT(lap, fftSize, mode = 2)
        val mag         = fft.complex.mag
        val mel         = MelFilter(mag, fftSize/2, bands = numMel,
          minFreq = minFreq, maxFreq = maxFreq, sampleRate = SR)
        val mfcc        = DCT_II(mel.log, numMel, numCoef, zero = 0 /* 1 */)
        mfcc
      }

      def mkPhaseSig(span: Span): GE = {
        val in0         = mkPhIn()
        val in1         = /*if (span.start == 0L) in0 else*/ in0.drop(span.start)
        val in          = in1.take(span.length)
        val mfcc        = mkMatrix(in)
        RepeatWindow(mfcc, size = covSize, num = runSteps)
      }

      def mkDbSig(span: Span): GE = {
        val in0         = mkDbIn()
        val in1         = /*if (span.start == 0L) in0 else*/ in0.drop(span.start)
        val in          = in1.take(span.length)
        val mfcc        = mkMatrix(in)
        val mfccSlid    = Sliding(mfcc, size = covSize, step = numCoef)
        mfccSlid
      }

      val sigPunchIn  = mkPhaseSig(punchIn )
      val sigPunchOut = mkPhaseSig(punchOut)

      //    val numSteps    = numFrames / stepSize
      //    val numCov      = numSteps - (2 * sideLen)
      //    val numCov1     = numSteps - sideLen - spaceLen

      val sigDbIn     = mkDbSig(dbSpanIn )
      val sigDbOut    = mkDbSig(dbSpanOut)
      val covIn       = Pearson(sigDbIn , sigPunchIn , covSize)
      val covOut      = Pearson(sigDbOut, sigPunchOut, covSize)

      val keys        = (covIn + covOut).take(runSteps)
      //    Length(BufferDisk(covNeg)).poll(0, "covNeg.length")
      //    Length(BufferDisk(wither)).poll(0, "wither.length")

      //    println(s"runSteps: $runSteps")
      //    Length(BufferDisk(keys)).poll(0, "key.length")

      //    Frames(sigDbIn ).poll(Metro(48000), "sigDbIn ")
      //    Frames(sigDbOut).poll(Metro(48000), "sigDbOut")

      //    AudioFileOut(file("/data/temp/match.aif"), AudioFileSpec(numChannels = 1, sampleRate = SR), in = keys)

      val keysEl      = keys.elastic()
      val values      = Frames(keysEl) - 1
      val top         = PriorityQueue(keysEl, values, size = 1)    // highest covariances mapped to frames
      val startF      = (top * stepSize) :+ 0L
      // startF.poll(0, "RESULT")
      //    Length(startF).poll(0, "RESULT-LEN")
      MkLong("result", startF)
    }

    f
  }

  def mkCtlSelectOverwrite[S <: Sys[S]](
                                         fscOvrSelect: stm.Obj[S],
                                         cuePh: proc.AudioCue.Obj[S],
                                         // phCount: IntObj[S],
                                       )
                                       (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("select"   , fscOvrSelect)
    c.attr.put("phrase"   , cuePh)
    // c.attr.put("ph-count" , phCount)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.synth.proc.ExImport._

    c.setGraph {
      val r = ThisRunner()

      val resSpan   = "span"      .attr[Span]
      val resNewLen = "new-length".attr[Long]

      val ph0   = "phrase".attr[AudioCue](AudioCue.Empty())
      val len0  = ph0.numFrames
      val SR    = 48000.0

      val minPhaseDur =   3.0
      val minPhInsDur =   1.5 // 3.0
      val maxPhaseDur =  30.0 // 150.0
      // val minStabDur  =  10.0
      val minPhaseLen = (SR * minPhaseDur).toLong
      val minPhInsLen = (SR * minPhInsDur).toLong
      val maxPhaseLen = (SR * maxPhaseDur).toLong

      // val pDur = len0 / SR // framesToSeconds(len0)
      // val mStretch = If (pDur <= minPhaseDur) Then {
      //   stretchMotion.set(stretchGrow)
      //   stretchGrow
      //   ???
      // } ElseIf (pDur >= maxPhaseDur) Then {
      //   stretchMotion.set(stretchShrink)
      //   stretchShrink
      //   ???
      // } ElseIf (pDur > minStabDur && random.nextDouble() < stableDurProb) Then {
      //   stretchMotion.set(stretchStable)
      //   stretchStable
      //   ???
      // } Else {
      //   stretchMotion()
      //   ???
      // }
      //
      // val fStretch  = mStretch.step()
      // val useBound  = random.nextDouble() <= ovrBoundaryProb
      // val boundEnd  = useBound && random.nextDouble() > 0.667
      // val jitAmt    = random.nextDouble()

      // val spaceDur  = spaceDurMotion.step()
      //
      // val fut = If (len0 > minPhaseLen) Then {
      //   SelectOverwrite(ph0.f, ctlCfg, spaceDur = spaceDur)
      //   ???
      // } Else {
      //   txFutureSuccessful(Span(0L, 0L))
      //   ???
      // }

      // val spaceDurMotion  : Motion = Motion.walk(1.2, 2.4, 0.1)

      // val stretchStable   : Motion = Motion.linexp(Motion.walk(0, 1, 0.1), 0, 1, 1.0 / 1.1, 1.1)
      // val stretchGrow     : Motion = Motion.walk(1.2, 2.0, 0.2)
      // val stretchShrink   : Motion = Motion.walk(0.6, 0.95, 0.2)
      val fStretch = 1.1 /*1.0*/ : Ex[Double] // XXX TODO

      val useBound = false  : Ex[Boolean] // XXX TODO
      val boundEnd = false  : Ex[Boolean] // XXX TODO
      val jitAmt   = 0.25   : Ex[Double]  // XXX TODO

      val spaceDur = (1.2 * 1.41) : Ex[Double] // XXX TODO
      val spanStart0  = Var[Long]
      val spanStop0   = Var[Long]

      val rSelect = Runner("select")

      val selectDone      = rSelect.stoppedOrDone
      val selectFail      = rSelect.failed

      val phCueIn         = "phrase".attr[AudioCue](AudioCue.Empty())

      val phFileIn        = phCueIn.artifact
      // val phCount         = "ph-count".attr(0)
      val loadBang        = LoadBang()
      // val phCount1        = phCount.latch(loadBang) + (1: Ex[Int])
      // val phFileOut       = phFileIn.replaceName("ph" ++ phCount1.toStr ++ ".aif")

      val actSelect: Ex[Act] = {
        val recRun: Act = rSelect.runWith(
          "space-dur" -> spaceDur,
          "in"        -> phFileIn,
          "start"     -> spanStart0,
          "stop"      -> spanStop0,
        )

        Act(
          PrintLn("run select"),
          recRun,
        )
      }

      val span0 = Span(spanStart0, spanStop0)
      val span1 = If (!useBound) Then span0 Else {
        If (boundEnd) Then Span(len0 - span0.length, len0) Else Span(0L, span0.length)
      }

      def jitter(in: Ex[Span], r: Ex[Double], secs: Ex[Double], minStart: Ex[Long], maxStop: Ex[Long]): Ex[Span] = {
        val secs2   = secs * 2
        val dFrames = (r * secs2 - secs * SR).toLong
        val d       = (minStart - in.start) max ((maxStop - in.stop) min dFrames)
        Span(in.start + d, in.stop + d)
      }

      val span        = jitter(span1, r = jitAmt, secs = 0.2, minStart = 0L, maxStop = len0)
      val newLength0  = minPhInsLen max (span.length * fStretch + (0.5: Ex[Double])).toLong
      val newDiff0    = newLength0 - span.length
      val len1        = minPhaseLen max (maxPhaseLen min (len0 + newDiff0))
      val newDiff1    = len1 - len0
      val newLength   = newDiff1 + span.length

      val actDone: Ex[Act] =
        Act(
          PrintLn("selected - start0: " ++ spanStart0.toStr ++
            " ; stop0: " ++ spanStop0.toStr ++
            " ; span: " ++ span.toStr ++
            " ; newLength: " ++ newLength.toStr
          ),
          resSpan   .set(span),
          resNewLen .set(newLength),
          r.done,
        )

      loadBang    ---> actSelect
      selectDone  ---> actDone
      selectFail  ---> r.fail("overwrite-select failed")

      // fut.map { span0 =>
      //   val span1       = if (!useBound) span0 else {
      //     if (boundEnd) Span(len0 - span0.length, len0)
      //     else          Span(0L, span0.length)
      //   }
      //   val span        = jitter(span1, r = jitAmt, secs = 0.2f, minStart = 0L, maxStop = len0)
      //   val newLength0  = max(minPhInsLen, (span.length * fStretch + 0.5).toLong)
      //   val newDiff0    = newLength0 - span.length
      //   val len1        = max(minPhaseLen, min(maxPhaseLen, len0 + newDiff0))
      //   val newDiff1    = len1 - len0
      //   val newLength   = newDiff1 + span.length
      //   val instr       = OverwriteInstruction(span, newLength = newLength)
      //   log(f"phSelectOverwrite() yields $instr; fStretch $fStretch%g, spaceDur $spaceDur%g")
      //   instr
      // }
    }

    c
  }

  def mkCtlFillDb[S <: Sys[S]](pQueryRadioRec: stm.Obj[S], pAppendDb: stm.Obj[S],
                               artTmp: artifact.Artifact[S], cueDb: proc.AudioCue.Obj[S],
                               dbCount: IntObj[S],
                              )
                              (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("query-radio-rec", pQueryRadioRec)
    c.attr.put("append-db"      , pAppendDb)
    c.attr.put("database"       , cueDb)
    c.attr.put("db-count"       , dbCount)
    pQueryRadioRec.attr.put("out", artTmp)  // XXX TODO `runWith` not yet supported by Proc
    mkObjIn(pQueryRadioRec, "dur", DEFAULT_VERSION) {
      DoubleObj.newVar[S](0.0)
    }

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.synth.proc.ExImport._

    c.setGraph {
      val r               = ThisRunner()
      val dbCueIn         = "database".attr[AudioCue](AudioCue.Empty())
      val recFile         = Artifact("query-radio-rec:out")
      val rQueryRadioRec  = Runner("query-radio-rec")
      val rAppendDb       = Runner("append-db")
      val recDur          = "query-radio-rec:dur"   .attr(0.0)
      val captFrames      = "append-db:capt-frames" .attr(0L)

      val SR              = 48000.0
      val dbTargetLen     = (SR * 180).toLong
      val maxCaptureLen   = (SR *  12).toLong  // 20
      val minCaptureLen   = (SR *   4).toLong

      val recDone         = rQueryRadioRec.stoppedOrDone
      val recFail         = rQueryRadioRec.failed
      val appendDone      = rAppendDb     .stoppedOrDone
      val appendFail      = rAppendDb     .failed

      val dbFileIn        = dbCueIn.artifact
      val appendDbIn      = Artifact("append-db:in-db")
      val appendDbOut     = Artifact("append-db:out-db")
      val appendAppIn     = Artifact("append-db:in-app")
      val dbCount         = "db-count".attr(0)
      val loadBang        = LoadBang()
      val dbCount1        = dbCount.latch(loadBang) + (1: Ex[Int])
      val dbFileOut       = dbFileIn.replaceName("db" ++ dbCount1.toStr ++ ".aif")

      val actAppend = Act(
        PrintLn("append rec"),
        appendDbIn  .set(dbFileIn ),  // in-db
        appendDbOut .set(dbFileOut),  // out-db
        appendAppIn .set(recFile),    // in-app
        rAppendDb.run
      )

      val actRec: Ex[Act] = {
        val dbLen = dbCueIn.numFrames
        val captLen: Ex[Long] = maxCaptureLen min (dbTargetLen - dbLen)
        If (captLen < minCaptureLen) Then {
          Act(
            PrintLn("(db long enough)"), r.done   // actAppend
          )
        } Else {
          val captSec     = captLen / SR
          val ts          = TimeStamp()
          val recName     = ts.format("'rec_'yyMMdd'_'HHmmss'_'SSS'.irc'")
          val recFileNew  = recFile.replaceName(recName)
          // log(f"dbFill() - capture dur $captSec%g sec")
          // XXX TODO `runWith` not yet supported by Proc
          // val recRun: Act = queryRadioRec.runWith(
          //   "dur" -> captSec,
          //   "out" -> recFileNew,
          // )
          val recRun: Act = rQueryRadioRec.run
          Act(
            PrintLn("run rec"),
            recFile   .set(recFileNew),
            recDur    .set(captSec),
            captFrames.set(captLen),
            recRun,
          )
        }
      }

      val specDbOut = AudioFileSpec.read(dbFileOut)
        .getOrElse(AudioFileSpec.Empty())
      val dbCueOut  = AudioCue(dbFileOut, specDbOut)

      val actDone = Act(
        PrintLn("append done."),
        dbCount.set(dbCount1),
        dbCueIn.set(dbCueOut),
        PrintLn(dbCueIn.toStr),
        r.done
      )

      loadBang    ---> actRec
      recDone     ---> actAppend
      recFail     ---> r.fail("query-radio-rec failed")
      appendDone  ---> actDone
      appendFail  ---> r.fail("database-append failed")

      // rQueryRadioRec.state.changed ---> PrintLn(rQueryRadioRec.state.toStr)
    }

    c
  }

  def mkFScInitialize[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph._

    f.setGraph {
      val SR  = 48000.0
      val sig = 0.0: GE // somehow can't write an empty file
      AudioFileOut("out-db", sampleRate = SR, in = sig)
      AudioFileOut("out-ph", sampleRate = SR, in = sig)
    }
    f
  }

  def mkWgtInitialize[S <: Sys[S]](
                                    fscInit: stm.Obj[S],
                                    locBase: artifact.ArtifactLocation[S],
                                    cueDb: proc.AudioCue.Obj[S],
                                    cuePh: proc.AudioCue.Obj[S],
                                    artTmp: artifact.Artifact[S],
                                    dbCount    : IntObj[S],
                                    phCount    : IntObj[S],
                                  )(implicit tx: S#Tx): proc.Widget[S] = {
    val w = proc.Widget[S]()
    w.attr.put("edit-mode", BooleanObj.newVar(false))
    w.attr.put("init-fsc" , fscInit)
    w.attr.put("database" , cueDb)
    w.attr.put("phrase"   , cuePh)
    w.attr.put("base"     , locBase)
    w.attr.put("tmp-file" , artTmp)
    w.attr.put("db-count" , dbCount)
    w.attr.put("ph-count" , phCount)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    import de.sciss.synth.proc.ExImport._

    w.setGraph {
      val pf = PathField()
      pf.mode = PathField.Folder
      pf.value <--> ArtifactLocation("base")

      val rFSc    = Runner("init-fsc")
      val aDb0    = Artifact("init-fsc:out-db" )
      val aPh0    = Artifact("init-fsc:out-ph" )
      val recFile = Artifact("tmp-file")
      val dbCueIn = "database".attr[AudioCue](AudioCue.Empty())
      val phCueIn = "phrase"  .attr[AudioCue](AudioCue.Empty())
      val dbCount = "db-count".attr(0)
      val phCount = "ph-count".attr(0)

      val p = GridPanel(
        Label("Sound file base directory:"),
        pf,
        Empty(),
        Label(
          "<html><body>Initialization creates three<p>" +
            "sub-directories here: 'db', 'ph', 'tmp'.<p>" +
            "Make sure there is enough free space."
        )
      )

      p.border          = Border.Empty(4)
      p.columns         = 2
      p.compactColumns  = true

      val baseDir = pf.value()
      val dbDir   = baseDir / "db"
      val phDir   = baseDir / "ph"
      val tmpDir  = baseDir / "tmp"

      val ggInit = Button("Initialize")
      ggInit.clicked ---> Act(
        dbDir.mkDir,
        phDir.mkDir,
        tmpDir.mkDir,
        aDb0.set(dbDir / "db0.aif"),
        aPh0.set(phDir / "ph0.aif"),
        rFSc.run
      )

      rFSc.failed ---> PrintLn(rFSc.messages.mkString("Init failed: ", "\n", ""))
      rFSc.done ---> Act(
        dbCueIn.set(AudioCue(aDb0, AudioFileSpec.Read(aDb0).getOrElse(AudioFileSpec.Empty()))),
        phCueIn.set(AudioCue(aPh0, AudioFileSpec.Read(aPh0).getOrElse(AudioFileSpec.Empty()))),
        recFile.set(tmpDir / "rec.irc"),
        dbCount.set(0),
        phCount.set(0),
        PrintLn("Initialized.")
      )

      val bp = BorderPanel(
        north = p,
        south = FlowPanel(ggInit)
      )

      bp.border          = Border.Empty(4)
      bp
    }

    w
  }
}
