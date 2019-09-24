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
import de.sciss.lucre.stm
import de.sciss.lucre.artifact
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, IntObj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import de.sciss.synth.proc
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.Workspace
import de.sciss.writingsimultan.BuilderUtil._

object Builder {
  val DEFAULT_VERSION = 1

  protected def any2stringadd: Any = ()

  def apply[S <: Sys[S]](audioBaseDir: File)(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val dbDir     = audioBaseDir / "db"
    val tmpDir    = audioBaseDir / "tmp"
    val dbFile0   = dbDir / "db0.aif"
    val tmpFile0  = tmpDir / "rec.irc"
    tx.afterCommit {
      dbDir .mkdirs()
      tmpDir.mkdirs()
    }

    val r             = workspace.root
    val fAux          = mkFolder(r, "aux")
    val loc           = mkObj[S, artifact.ArtifactLocation](fAux, "base", DEFAULT_VERSION) {
      artifact.ArtifactLocation.newVar[S](audioBaseDir)
    }
    val artTmp = mkObj[S, artifact.Artifact](fAux, "rec", DEFAULT_VERSION) {
      artifact.Artifact[S](loc, tmpFile0)
    }
    val cueDb = mkObj[S, proc.AudioCue.Obj](fAux, "database", DEFAULT_VERSION) {
//      val artDb = artifact.Artifact[S](loc, dbFile0)
      val spec0 = synth.io.AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, numChannels = 1, sampleRate = 48000.0)
      tx.afterCommit {
        // create empty file
        if (!dbFile0.exists()) {
          val af = synth.io.AudioFile.openWrite(dbFile0, spec0)
          af.close()
        }
      }
//      proc.AudioCue.Obj[S](artDb, spec0, 0L, 1.0)
      // N.B.: Obj.Bridge can only do in-place update with Expr.Var!
      proc.AudioCue.Obj.newVar[S](
        proc.AudioCue(
          dbFile0 /*artDb*/, spec0, 0L, 1.0
        )
      )
    }

    val dbCount         = mkObj[S, IntObj   ](fAux, "db-count"        , DEFAULT_VERSION)(IntObj.newVar[S](0))
    val pQueryRadioRec  = mkObj[S, proc.Proc](fAux, "query-radio-rec" , DEFAULT_VERSION)(mkProcQueryRadioRec [S]())
    val pAppendDb       = mkObj[S, FScape   ](fAux, "database-append" , DEFAULT_VERSION)(mkFScAppendDb       [S]())
    mkObj[S, proc.Control](fAux, "fill-database", DEFAULT_VERSION) {
      mkCtlFillDb(pQueryRadioRec = pQueryRadioRec, pAppendDb = pAppendDb, artTmp = artTmp, cueDb = cueDb,
        dbCount = dbCount)
    }
    mkObj[S, proc.Control ](r, "main" , DEFAULT_VERSION)(mkControlMain())
    mkObj[S, proc.Widget  ](r, "reset", DEFAULT_VERSION)(mkWgtReset(cueDb = cueDb, dbCount = dbCount))
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
      val dur = "dur".ir // (1.0)
      dur.poll(0, "query-radio-rec dur")
      DiskOut.ar("out", in)
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

      val cat: GE = If (fdLen.elastic(4) sig_== 0) Then {
        //  DC(0).take(1).poll(0, "branch-1")
        inDb ++ inApp
      } Else {
        //  DC(0).take(1).poll(0, "branch-2")
        val preLen  = len0 - fdLen
        val pre     = inDb.take(preLen)
        val cross0  = inDb.drop(preLen) * Line(1, 0, fdLen).sqrt
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

  def mkWgtReset[S <: Sys[S]](
                               cueDb: proc.AudioCue.Obj[S], dbCount: IntObj[S]
                             )
                             (implicit tx: S#Tx): proc.Widget[S] = {
    val w = proc.Widget[S]()
    w.attr.put("database" , cueDb)
    w.attr.put("db-count" , dbCount)
    w.attr.put("edit-mode" , BooleanObj.newVar(false))

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.swing.graph._
    import de.sciss.synth.proc.ExImport._

    w.setGraph {
      val dbCueIn   = "database".attr[AudioCue](AudioCue.Empty())
      val dbCount   = "db-count".attr(0)
      val bReset    = Bang()
      val bInfo     = Bang()

      val dbFileIn  = dbCueIn.artifact
      val dbFileOut = dbFileIn.replaceName("db0.aif")
      bReset ---> Act(
        dbCount.set(0),
        dbCueIn.set(AudioCue(dbFileOut, AudioFileSpec.Empty())),
      )

      bInfo ---> Act(
        PrintLn("db-count = " ++ dbCount.toStr),
        PrintLn("db-cue   = " ++ dbCueIn.toStr),
      )

      val p = GridPanel(
        Label("Reset Database State:"),
        bReset,
        Label("Info:"),
        bInfo,
      )
      p.columns = 2
      p
    }
    w
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
    c.attr.put("temp-file"      , artTmp)
    c.attr.put("db-count", dbCount)
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

      // 0 stopped, 1 prepare, 2 prepared, 3 run, 4 done, 5 fail
      val recState        = rQueryRadioRec.state
      val recDone         = ((recState sig_== 4) || (recState sig_== 0)).toTrig
      val recFail         = (rQueryRadioRec.state sig_== 5).toTrig

      val appendState     = rAppendDb.state
      val appendDone      = ((appendState sig_== 4) || (appendState sig_== 0)).toTrig
      val appendFail      = (rAppendDb.state sig_== 5).toTrig

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
          actAppend // r.done
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
            PrintLn("start rec"),
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

      rQueryRadioRec.state.changed ---> PrintLn(rQueryRadioRec.state.toStr)
    }

    c
  }

  def mkControlMain[S <: Sys[S]]()(implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    import de.sciss.lucre.expr.graph._

    c.setGraph {
      // main entrance point to the installation
      LoadBang() ---> PrintLn("---- Writing (simultan) ----")
    }

    c
  }

  def mkFScapeFindPauses[S <: Sys[S]]()(implicit tx: S#Tx): FScape[S] = {
    val f = FScape[S]()
    import de.sciss.fscape.graph.{AudioFileIn => _, AudioFileOut => _, _}
    import de.sciss.fscape.lucre.graph.Ops._
    import de.sciss.fscape.lucre.graph._

    // this is a test from Mäanderungen --- to be removed / changed
    f.setGraph {
      DC(17).take(1).poll(0, "VERSION")
      val threshLoud    = 15.0
      val in0           = AudioFileIn("in")
      val in            = Mix.MonoEqP(in0.elastic(7)) // XXX TOD -- Mix should be taking care of this
      val sampleRate    = in0.sampleRate
      val inFrames      = in0.numFrames

      // loudness
      val winLoud       = (0.2 * sampleRate).floor
      val stepLoud      = (winLoud/4).floor
      val framesLoud    = ((inFrames + stepLoud - 1) / stepLoud).floor
      val slidLoud      = Sliding(in, size = winLoud, step = stepLoud)
      val loud          = Loudness(slidLoud, sampleRate = sampleRate, size = winLoud, spl = 70, diffuse = 1)

      // pauses
      val medianLoud    = SlidingPercentile(loud, len = 5)
      val foreground    = medianLoud > threshLoud
      val fgDif         = Differentiate(foreground)
      val toBack        = fgDif sig_== -1
      val toFront       = fgDif sig_== +1
      val offLoud       = Frames(fgDif)
      val pauseStart    = FilterSeq(offLoud, toBack) // .dropRight(1) -- not yet implemented
      val pauseStop     = FilterSeq(offLoud, toFront  ).drop(1)
      val spans         = ((pauseStart zip pauseStop) - 1) * stepLoud
      val srLoud        = sampleRate / stepLoud

      // pitch
      val isMale        = "is-male".attr(0) // determine pitch tracking register

      val (pitch, srPitch, framesPitch) = {
        val minPitch            = 100.0 - (isMale *  40.0) // if (isMale)  60.0 else 100.0 // 100.0
        val maxPitch            = 320.0 - (isMale * 120.0) // if (isMale) 200.0 else 320.0 // 1000.0
        val voicingThresh       = 0.45
        val silenceThresh       = 0.03
        val octaveCost          = 0.01
        val octaveJumpCost      = 0.35
        val voicedUnvoicedCost  = 0.14
        val numTracks           = 15

        val _pch = PitchAC(in, sampleRate = sampleRate, pitchMin = minPitch, pitchMax = maxPitch,
          voicingThresh = voicingThresh, silenceThresh = silenceThresh, octaveCost = octaveCost,
          octaveJumpCost = octaveJumpCost, voicedUnvoicedCost = voicedUnvoicedCost,
          numCandidates = numTracks)

        val stepPitch   = _pch.stepSize
        val _frames     = ((inFrames + stepPitch - 1) / stepPitch).floor

        val _sr = sampleRate / stepPitch
        (_pch, _sr, _frames)
      }

      // write
      val writtenLoud   = AudioFileOut("loud"   , loud , sampleRate = srLoud  )
      val writtenPitch  = AudioFileOut("pitch"  , pitch, sampleRate = srPitch )
      /* val writtenSpans  = */ AudioFileOut("pauses" , spans)
      // val writtenAll    = writtenLoud ++ writtenPitch ++ writtenSpans

      Progress(writtenLoud  / framesLoud  , Metro(srLoud  ), "loudness")
      Progress(writtenPitch / framesPitch , Metro(srPitch ), "pitch"   )

      // Action(Done(writtenAll), "done")
    }
    f
  }
}
