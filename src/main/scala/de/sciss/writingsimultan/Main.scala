/*
 *  Main.scala
 *  Writing (simultan)
 *
 *  Copyright (c) 2019-2020 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.writingsimultan

import de.sciss.file._
import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.{Durable, SoundProcesses, Widget, Workspace}

object Main {
  case class Config(wsFile: File /*, audioBaseDir: File*/)

  type S = Durable

  def main(args: Array[String]): Unit = {
    val projectDir    = file("/data/projects/WritingSimultan")
    val wsFile        = projectDir / "writing-simultan.mllt"
//    val audioBaseDir  = projectDir / "audio_work"
    implicit val cf: Config = Config(
      wsFile        = wsFile,
//      audioBaseDir  = audioBaseDir,
    )
    run()
  }

  def run()(implicit config: Config): Unit = {
    SoundProcesses.init()
    FScape        .init()
    Widget        .init()

    val store = BerkeleyDB.factory(config.wsFile, createIfNecessary = true)
    implicit val ws: Workspace[S] = Workspace.Durable.empty(config.wsFile, store)
    ws.cursor.
      step { implicit tx =>
      Builder(/*audioBaseDir = config.audioBaseDir*/)
    }
    ws.close()
    sys.exit()
  }
}
