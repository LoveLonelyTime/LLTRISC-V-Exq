package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._
import bergamot.utils.ReadWriteSRAM

/*
 * On-chip ROM
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ROM
  *
  * @param size
  *   Size
  * @param base
  *   Base address
  * @param img
  *   Image file
  */
class ROM(size: Int, base: String, img: String = "") extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
  })

  io.axi <> new AXIMasterIO().zero

  private val mem = Module(new ReadWriteSRAM(UInt(32.W))(size, img))

  mem.io.dataIn := 0.U
  mem.io.addr := 0.U
  mem.io.write := false.B

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    mem.io.addr := getWordAddress(io.axi.ARADDR - base.U)
  }

  io.axi.RVALID := true.B
  io.axi.RDATA := mem.io.dataOut
  io.axi.RRESP := 0.U

  // Disable write
  io.axi.AWREADY := true.B
  io.axi.WREADY := true.B
  io.axi.BVALID := true.B
  io.axi.BRESP := "b10".U
}
