package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._
import bergamot.utils.DoublePortSRAM

/*
 * Verilator virtual write host (riscv-tests)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Virtual write host
  *
  * @param base
  *   Base address
  */
class VirtualWriteHost(base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
    val accept = Output(Bool())
  })

  io.axi <> new AXIMasterIO().zero

  private val writeAddress = RegInit(0.U(32.W))

  private val writeHostReg = RegInit(VecInit.fill(2)(0.U(32.W)))

  io.accept := writeHostReg(0) === 1.U

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
  }

  io.axi.RVALID := true.B
  io.axi.RDATA := 0.U
  io.axi.RRESP := 0.U

  when(io.axi.AWVALID) {
    io.axi.AWREADY := true.B
    writeAddress := io.axi.AWADDR - base.U
  }

  when(io.axi.WVALID) {
    io.axi.WREADY := true.B
    writeHostReg(getWordAddress(writeAddress)) := io.axi.WDATA
  }

  io.axi.BVALID := true.B
  io.axi.BRESP := 0.U
}
