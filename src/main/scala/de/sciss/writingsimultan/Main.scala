/*
 *  Main.scala
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
import de.sciss.fscape.lucre.FScape
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.{Durable, SoundProcesses, Workspace}

object Main {
  case class Config(ws: File)

  type S = Durable

  def main(args: Array[String]): Unit = {
    implicit val cf: Config = Config(file("/data/projects/WritingSimultan/writing-simultan.mllt"))
    run()
  }

  def run()(implicit config: Config): Unit = {
    SoundProcesses.init()
    FScape        .init()

    val store = BerkeleyDB.factory(config.ws, createIfNecessary = true)
    implicit val ws: Workspace[S] = Workspace.Durable.empty(config.ws, store)
    ws.cursor.step { implicit tx =>
      Builder()
    }
    ws.close()
    sys.exit()
  }
}
