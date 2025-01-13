package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.bus.AXIMasterIO

import bergamot.utils.ChiselUtils._

/*
 * Verilator virtual RAM
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Virtual RAM
  *
  * @param base
  *   Base address
  */
class VirtualRAM(base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
    val rdAddress = Output(UInt(32.W))
    val rdData = Input(UInt(32.W))
    val wrAddress = Output(UInt(32.W))
    val wrData = Output(UInt(32.W))
    val wrStrobe = Output(UInt(4.W))
  })

  io.axi <> new AXIMasterIO().zero

  private val readAddressReg = RegInit(0.U(32.W))

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    readAddressReg := io.axi.ARADDR - base.U
  }

  io.rdAddress := readAddressReg

  io.axi.RVALID := true.B
  io.axi.RDATA := io.rdData
  io.axi.RRESP := 0.U

  private val writeAddressReg = RegInit(0.U(32.W))

  io.wrAddress := writeAddressReg

  when(io.axi.AWVALID) {
    io.axi.AWREADY := true.B
    writeAddressReg := io.axi.AWADDR - base.U
  }

  io.wrData := io.axi.WDATA
  io.wrStrobe := 0.U
  when(io.axi.WVALID) {
    io.axi.WREADY := true.B
    io.wrStrobe := io.axi.WSTRB
  }

  io.axi.BVALID := true.B
  io.axi.BRESP := 0.U
}
