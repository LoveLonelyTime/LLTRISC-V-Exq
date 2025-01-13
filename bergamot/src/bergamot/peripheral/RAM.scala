package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._
import bergamot.utils.DoublePortSRAM

/*
 * On-chip RAM
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** RAM
  *
  * @param size
  *   Size
  * @param base
  *   Base Address
  */
class RAM(size: Int, base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
  })

  io.axi <> new AXIMasterIO().zero

  // 4-way 8-bits ROMs
  private val mem = Seq.fill(4)(Module(new DoublePortSRAM(UInt(8.W))(size)))
  private val writeAddress = RegInit(0.U(32.W))

  mem.foreach { item =>
    item.io.wrAddr := 0.U
    item.io.rdAddr := 0.U
    item.io.wrData := 0.U
    item.io.write := false.B
  }

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    mem.foreach(_.io.rdAddr := getWordAddress(io.axi.ARADDR - base.U))
  }

  io.axi.RVALID := true.B
  io.axi.RDATA := mem(3).io.rdData ## mem(2).io.rdData ## mem(1).io.rdData ## mem(0).io.rdData
  io.axi.RRESP := 0.U

  when(io.axi.AWVALID) {
    io.axi.AWREADY := true.B
    writeAddress := io.axi.AWADDR - base.U
  }

  when(io.axi.WVALID) {
    io.axi.WREADY := true.B
    mem.foreach(_.io.wrAddr := getWordAddress(writeAddress))

    mem(0).io.wrData := io.axi.WDATA(7, 0)
    mem(1).io.wrData := io.axi.WDATA(15, 8)
    mem(2).io.wrData := io.axi.WDATA(23, 16)
    mem(3).io.wrData := io.axi.WDATA(31, 24)

    mem.zipWithIndex.foreach { case (lane, i) => lane.io.write := io.axi.WSTRB(i) }
  }

  io.axi.BVALID := true.B
  io.axi.BRESP := 0.U
}
