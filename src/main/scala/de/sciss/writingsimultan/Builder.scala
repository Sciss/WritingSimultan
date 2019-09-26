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
    val phDir     = audioBaseDir / "ph"
    val tmpDir    = audioBaseDir / "tmp"
    val dbFile0   = dbDir / "db0.aif"
    val phFile0   = phDir / "ph0.aif"
    val tmpFile0  = tmpDir / "rec.irc"
    tx.afterCommit {
      dbDir .mkdirs()
      phDir .mkdirs()
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
    /*val pFillDb         =*/ mkObj[S, proc.Control](fAux, "fill-database", DEFAULT_VERSION) {
      mkCtlFillDb(pQueryRadioRec = pQueryRadioRec, pAppendDb = pAppendDb, artTmp = artTmp, cueDb = cueDb,
        dbCount = dbCount)
    }

    val phCount         = mkObj[S, IntObj   ](fAux, "ph-count"        , DEFAULT_VERSION)(IntObj.newVar[S](0))
    val cuePh = mkObj[S, proc.AudioCue.Obj](fAux, "phrase", DEFAULT_VERSION) {
      val spec0 = synth.io.AudioFileSpec(AudioFileType.AIFF, SampleFormat.Float, numChannels = 1, sampleRate = 48000.0)
      tx.afterCommit {
        // create empty file
        if (!phFile0.exists()) {
          val af = synth.io.AudioFile.openWrite(phFile0, spec0)
          af.close()
        }
      }
      // N.B.: Obj.Bridge can only do in-place update with Expr.Var!
      proc.AudioCue.Obj.newVar[S](
        proc.AudioCue(
          phFile0 /*artPh*/, spec0, 0L, 1.0
        )
      )
    }

    val pOverwriteSelect = mkObj[S, FScape](fAux, "overwrite-select" , DEFAULT_VERSION)(mkFScOverwriteSelect[S]())
    /*val pOverwrite = */ mkObj[S, proc.Control](fAux, "overwrite", DEFAULT_VERSION) {
      mkCtlOverwrite(pOverwriteSelect = pOverwriteSelect, cuePh = cuePh,
        phCount = phCount)
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

      val fftSize   : Int     = 1024 // 2048
      val stepDiv   : Int     = 4
      val numMel    : Int     = 42
      val numCoef   : Int     = 21
      val sideDur   : Double  = 0.25
      val minFreq   : Double  = 100
      val maxFreq   : Double  = 14000
      val witherTgt : Double  = 0.0012 / 30   // a threshold of 0.0012 in 30 iterations
      val WitheringConstant: Double = 0.0078125   // = 1.0/128

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
      val startF      = (top * stepSize) ++ DC(0)
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

  def mkCtlOverwrite[S <: Sys[S]](pOverwriteSelect: stm.Obj[S], cuePh: proc.AudioCue.Obj[S], phCount: IntObj[S],
                                 )
                                 (implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    c.attr.put("select"   , pOverwriteSelect)
    c.attr.put("phrase"   , cuePh)
    c.attr.put("ph-count" , phCount)

    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    import de.sciss.synth.proc.ExImport._

    c.setGraph {
      val r               = ThisRunner()
      // ---- select ----

      val ph0   = "phrase".attr[AudioCue](AudioCue.Empty())
      val len0  = ph0.numFrames
      val SR    = 48000.0

      val minPhaseDur =   3.0
      val maxPhaseDur =  30.0 // 150.0
      val minStabDur  =  10.0
      val minPhaseLen = (SR * minPhaseDur).toLong

      val pDur = len0 / SR // framesToSeconds(len0)
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

      val spaceDur = (1.2 * 1.41) : Ex[Double] // XXX TODO
      val spanStart0  = Var[Long]
      val spanStop0   = Var[Long]

      val rSelect = Runner("select")

      val selectDone      = rSelect.stoppedOrDone
      val selectFail      = rSelect.failed

      val phCueIn         = "phrase".attr[AudioCue](AudioCue.Empty())

      val phFileIn        = phCueIn.artifact
      val phCount         = "ph-count".attr(0)
      val loadBang        = LoadBang()
      val phCount1        = phCount.latch(loadBang) + (1: Ex[Int])
      val phFileOut       = phFileIn.replaceName("ph" ++ phCount1.toStr ++ ".aif")

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

      val actAdjust: Ex[Act] =
        Act(
          PrintLn("selected - start: " ++ spanStart0.toStr ++ " ; stop: " ++ spanStop0.toStr)
        )

      val actDone = Act(
        PrintLn("overwrite done."),
        r.done
      )

      loadBang    ---> actSelect
      selectDone  ---> actAdjust
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
    c.attr.put("temp-file"      , artTmp)
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
