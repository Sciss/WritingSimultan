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

import de.sciss.fscape.lucre.FScape
import de.sciss.fscape.lucre.MacroImplicits._
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.MacroImplicits._
import de.sciss.synth.proc.Workspace
import de.sciss.writingsimultan.BuilderUtil._

object Builder {
  val DEFAULT_VERSION = 1

  def apply[S <: Sys[S]]()(implicit tx: S#Tx, workspace: Workspace[S]): Unit = {
    val r             = workspace.root
//    val fAna          = mkFolder(r, "analysis")
//    mkObj[S, proc.Action](fAna, "find-pauses", DEFAULT_VERSION)(mkActionFindPauses[S]())
//    mkObj[S, proc.Action](fAna, "remove-meta", DEFAULT_VERSION)(mkActionRemoveMeta[S]())
    mkObj[S, proc.Control](r, "main", DEFAULT_VERSION)(mkControlMain())
  }

  protected def longWrapper: Any = ()

  def mkCtlDbFill[S <: Sys[S]]()(implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.synth.proc.ExImport._

    //    c.graph() = proc.Control.Graph {
    c.setGraph {
      val r             = ThisRunner()
      val dbFile        = "db-file".attr[AudioCue]
      val queryRadioRec = Runner("query-radio-rec")

      val SR            = 48000.0
      val dbTargetLen   = (SR * 180).toLong
      val maxCaptureLen = (SR *  12).toLong  // 20
      val minCaptureLen = (SR *   4).toLong

      val opt: Ex[Option[Act]] = dbFile.map { db0 =>
        //        val db0     = dbFile
        val len0    = db0.numFrames
        val captLen = maxCaptureLen min (dbTargetLen - len0)
        If (captLen < minCaptureLen) Then {
          r.done // done /*txFutureSuccessful(len0)*/
        } Else {
          val captSec     = (captLen / SR) // .toFloat
          // log(f"dbFill() - capture dur $captSec%g sec")
          // val fFileApp = Artifact.
//          val futFileApp: Ex[AudioCue] = ??? // client.queryRadioRec(captSec)
          val recRun: Act = queryRadioRec.runWith(
            "dur" -> captSec
          )
          val recDone = queryRadioRec.state.changed.filter(queryRadioRec.state sig_== 3)
          recDone ---> r.done
          recRun

          //            futFileApp.flatMap { fileApp =>
          //              val numFrames = min(fileApp.numFrames, captLen)
          //              atomic { implicit tx =>
          //                dbAppend(fileApp = fileApp.f, numFrames = numFrames).andThen {
          //                  case _ => atomic { implicit tx => fileApp.release() }
          //                }
          //              }
          //            }
        }
      }
      opt.getOrElse(r.fail("no database"))
    }

    c
  }

  def mkControlMain[S <: Sys[S]]()(implicit tx: S#Tx): proc.Control[S] = {
    val c = proc.Control[S]()
    import de.sciss.lucre.expr.graph._
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.synth.proc.ExImport._

//    c.setGraph {
    c.graph() = proc.Control.Graph {
      // main entrance point to the installation
      LoadBang() ---> PrintLn("---- Writing (simultan) ----")

      val dbFile = "db-file".attr[AudioCue]

      val queryRadioRec = Runner("query-radio-rec")

      /*

      StopSelf()
      DoneSelf()
      FailSelf()
      CompleteSelf(failure: Ex[String] = "")

       */

      def dbFill(done: Act): Act = {
        val SR            = 48000.0
        val dbTargetLen   = (SR * 180).toLong
        val maxCaptureLen = (SR *  12).toLong  // 20
        val minCaptureLen = (SR *   4).toLong

        val opt: Ex[Option[Act]] = dbFile.map { db0 =>
//        val db0     = dbFile
          val len0    = db0.numFrames
          val captLen = maxCaptureLen min (dbTargetLen - len0)
          If (captLen < minCaptureLen) Then {
            done /*txFutureSuccessful(len0)*/
          } Else {
            val captSec     = (captLen / SR) // .toFloat
            // log(f"dbFill() - capture dur $captSec%g sec")
            // val fFileApp = Artifact.
//            val futFileApp: Ex[AudioCue] = ??? // client.queryRadioRec(captSec)
            val recRun: Act = queryRadioRec.runWith(
              "dur" -> captSec
            )
            val recDone = queryRadioRec.state.changed.filter(queryRadioRec.state sig_== 3)
            recDone ---> done
            recRun

//            futFileApp.flatMap { fileApp =>
//              val numFrames = min(fileApp.numFrames, captLen)
//              atomic { implicit tx =>
//                dbAppend(fileApp = fileApp.f, numFrames = numFrames).andThen {
//                  case _ => atomic { implicit tx => fileApp.release() }
//                }
//              }
//            }
          }
        }
        opt.orNop
      }
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
