package bergamot.bus

import chisel3._
import chisel3.util._

/*
 * AXI4-Lite Bus
 *
 * ARM(R) AMBA(R) AXI Protocol
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** AXI Specification
  */
object AXISpec {
  val addressWidth = 32
  val dataWidth = 32
  val portWidth = 3
  val respWidth = 2
}

/** AXI4-Lite master interface
  */
class AXIMasterIO extends Bundle {
  // Write request channel (AW)
  val AWVALID = Output(Bool())
  val AWREADY = Input(Bool())
  val AWADDR = Output(UInt(AXISpec.addressWidth.W))
  val AWPROT = Output(UInt(AXISpec.portWidth.W))

  // Write data channel (W)
  val WVALID = Output(Bool())
  val WREADY = Input(Bool())
  val WDATA = Output(UInt(AXISpec.dataWidth.W))
  val WSTRB = Output(UInt((AXISpec.dataWidth / 8).W))

  // Write response channel (B)
  val BVALID = Input(Bool())
  val BREADY = Output(Bool())
  val BRESP = Input(UInt(AXISpec.respWidth.W))

  // Read request channel (AR)
  val ARVALID = Output(Bool())
  val ARREADY = Input(Bool())
  val ARADDR = Output(UInt(AXISpec.addressWidth.W))
  val ARPROT = Output(UInt(AXISpec.portWidth.W))

  // Read data channel (R)
  val RVALID = Input(Bool())
  val RREADY = Output(Bool())
  val RDATA = Input(UInt(AXISpec.dataWidth.W))
  val RRESP = Input(UInt(AXISpec.respWidth.W))
}
