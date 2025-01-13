package bergamot.interconnect

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * AXI interconnect
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** AXI interconnect
  *
  * @param memoryMap
  *   Memory map in ascend order
  */
class AXIInterconnect(memoryMap: Seq[String]) extends Module {
  val io = IO(new Bundle {
    val master = Flipped(new AXIMasterIO())
    val slaves = Vec(memoryMap.length, new AXIMasterIO())
  })

  private val memoryPairMap = memoryMap.zip(memoryMap.drop(1) :+ "#")

  io.master <> new AXIMasterIO().zero
  io.slaves.foreach(_ <> new AXIMasterIO().zero)

  private val readGrantReg = RegInit(VecInit.fill(memoryMap.length)(false.B))
  io.slaves.zip(readGrantReg).zip(memoryPairMap).foreach { case ((slave, grant), (startAddress, endAddress)) =>
    when(io.master.ARADDR >= startAddress.U && (if (endAddress == "#") true.B else io.master.ARADDR < endAddress.U)) {
      slave.ARADDR := io.master.ARADDR
      slave.ARPROT := io.master.ARPROT
      slave.ARVALID := io.master.ARVALID
      io.master.ARREADY := slave.ARREADY

      when(io.master.ARVALID && io.master.ARREADY) {
        grant := true.B
      }
    }
  }

  io.slaves.zip(readGrantReg).foreach { case (slave, granted) =>
    when(granted) {
      io.master.RDATA := slave.RDATA
      io.master.RVALID := slave.RVALID
      io.master.RRESP := slave.RRESP
      slave.RREADY := io.master.RREADY

      when(io.master.RVALID && io.master.RREADY) {
        granted := false.B
      }
    }
  }

  private val writeGrantReg = RegInit(VecInit.fill(memoryMap.length)(false.B))
  io.slaves.zip(writeGrantReg).zip(memoryPairMap).foreach { case ((slave, grant), (startAddress, endAddress)) =>
    when(io.master.AWADDR >= startAddress.U && (if (endAddress == "#") true.B else io.master.AWADDR < endAddress.U)) {
      slave.AWADDR := io.master.AWADDR
      slave.AWPROT := io.master.AWPROT
      slave.AWVALID := io.master.AWVALID
      io.master.AWREADY := slave.AWREADY

      when(io.master.AWVALID && io.master.AWREADY) {
        grant := true.B
      }
    }
  }

  io.slaves.zip(writeGrantReg).foreach { case (slave, granted) =>
    when(granted) {
      slave.WDATA := io.master.WDATA
      slave.WSTRB := io.master.WSTRB
      slave.WVALID := io.master.WVALID
      io.master.WREADY := slave.WREADY

      io.master.BRESP := slave.BRESP
      io.master.BVALID := slave.BVALID
      slave.BREADY := io.master.BREADY

      when(io.master.BVALID && io.master.BREADY) {
        granted := false.B
      }
    }
  }
}
