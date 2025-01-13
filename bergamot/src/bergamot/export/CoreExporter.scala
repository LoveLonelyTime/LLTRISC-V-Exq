package bergamot.export

import java.util.Scanner
import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import chisel3._
import chisel3.util._

import bergamot.core._

/*
 * Core exporter
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

object CoreExporter {
  private val BANNER = """
00000000  00000000 00000000   111111      000    00     00  0000000  11111111 
00     00 00       00     00 11    11    00 00   000   000 00     00    11    
00     00 00       00     00 11         00   00  0000 0000 00     00    11    
00000000  000000   00000000  11   1111 00     00 00 000 00 00     00    11    
00     00 00       00   00   11    11  000000000 00     00 00     00    11    
00     00 00       00    00  11    11  00     00 00     00 00     00    11    
00000000  00000000 00     00  111111   00     00 00     00  0000000     11    
"""

  private val THANK = """
----------------------------------------------
|       Thank you for using Bergamot!        |
| https://github.com/LoveLonelyTime/Bergamot |
|               BergamotCore                 |
|                                            |
----------------------------------------------
"""

  private def printVersionInfo() = {
    Files.readAllLines(Paths.get("VERSION")).forEach(println(_))
  }

  private val exportList = List(
    "Standard Bergamot core" -> (() => new BergamotCore(CoreConfig.default)),
    "Verilator test core" -> (() => new VerilatorTestCore())
  )

  private def selectExportList() = {
    exportList.zipWithIndex.foreach { case (core, id) =>
      println(s"${id + 1}. ${core._1}")
    }
    print("Please select the core you want to export: ")
    val id = new Scanner(System.in).nextInt()

    if (id < 1 || id > exportList.length) {
      println("Error ID! Exit!")
      None
    } else {
      Some(exportList(id - 1)._2)
    }
  }

  def main(args: Array[String]): Unit = {
    println(BANNER)
    printVersionInfo()
    selectExportList() match {
      case Some(core) => {
        emitVerilog(core(), args)
        println(THANK)
      }
      case None => ()
    }
  }
}
