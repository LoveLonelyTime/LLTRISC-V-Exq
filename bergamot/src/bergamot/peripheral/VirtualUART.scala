package bergamot.peripheral

import chisel3._
import chisel3.util._

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

import bergamot.bus.AXIMasterIO

/*
 * Verilator virtual UART (uart8250)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Virtual UART
  *
  * @param base
  *   Base address
  */
class VirtualUART(base: String) extends Module {
  val io = IO(new Bundle {
    val axi = Flipped(new AXIMasterIO())
    val dataOut = Output(UInt(8.W))
    val send = Output(Bool())
  })

  io.axi <> new AXIMasterIO().zero
  io.dataOut := 0.U
  io.send := false.B

  private val isSendingFlag = WireInit(false.B)

  private val readAddressReg = RegInit(0.U(32.W))

  when(io.axi.ARVALID) {
    io.axi.ARREADY := true.B
    readAddressReg := io.axi.ARADDR - base.U
  }

  io.axi.RVALID := true.B
  io.axi.RRESP := 0.U

  // Read logic
  when(readAddressReg === 4.U) { // UART_SCR_OFFSET UART_MSR_OFFSET UART_LSR_OFFSET UART_MCR_OFFSET
    val LSR = 0.U ## !isSendingFlag ## !isSendingFlag ## 0.U(4.W) ## 0.U
    io.axi.RDATA := 0.U(8.W) ## 0.U(8.W) ## LSR ## 0.U(8.W)
  }

  private val writeAddressReg = RegInit(0.U(32.W))

  when(io.axi.AWVALID) {
    io.axi.AWREADY := true.B
    writeAddressReg := io.axi.AWADDR - base.U
  }

  when(io.axi.WVALID) {
    io.axi.WREADY := true.B
    val data = io.axi.WDATA
    val strobe = io.axi.WSTRB

    // Write logic
    when(writeAddressReg === 0.U) {
      when(strobe(0)) { // UART_THR_OFFSET
        isSendingFlag := true.B
        io.dataOut := data(7, 0)
        io.send := true.B
      }
    }
  }

  io.axi.BVALID := true.B
  io.axi.BRESP := 0.U
}
