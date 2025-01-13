package bergamot.utils

import chisel3._
import chisel3.util._

import chisel3.util.experimental.loadMemoryFromFileInline

/*
 * Read write SRAM
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Standard synchronous memory
  *
  * This can be replaced with a special IP by FPGA or ASIC
  *
  * @param gen
  *   Type
  * @param size
  *   SRAM size
  * @param img
  *   Image file path
  */
class ReadWriteSRAM[T <: Data](gen: T)(size: Int, img: String = "") extends Module {
  val io = IO(new Bundle {
    val write = Input(Bool())
    val addr = Input(UInt(log2Ceil(size).W))
    val dataIn = Input(gen)
    val dataOut = Output(gen)
  })

  private val mem = SyncReadMem(size, gen)

  // I think this is a problem about dedup optimization
  // See https://github.com/chipsalliance/firrtl/issues/2168
  private val dedupBlock = WireInit(Math.abs(img.hashCode).U)

  io.dataOut := mem.read(io.addr)

  when(io.write) {
    mem.write(io.addr, io.dataIn)
  }

  if (img.trim().nonEmpty) {
    loadMemoryFromFileInline(mem, img)
  }
}

/** Double port SRAM
  *
  * @param gen
  *   Type
  * @param size
  *   SRAM size
  * @param img
  *   Image file path
  */
class DoublePortSRAM[T <: Data](gen: T)(size: Int, img: String = "") extends Module {
  val io = IO(new Bundle {
    val write = Input(Bool())
    val wrAddr = Input(UInt(log2Ceil(size).W))
    val rdAddr = Input(UInt(log2Ceil(size).W))
    val wrData = Input(gen)
    val rdData = Output(gen)
  })

  private val mem = SyncReadMem(size, gen)
  private val dedupBlock = WireInit(Math.abs(img.hashCode).U)

  private val wrDataReg = RegNext(io.wrData)
  private val doForwardReg = RegNext(io.wrAddr === io.rdAddr && io.write)

  private val memData = mem.read(io.rdAddr)

  when(io.write) {
    mem.write(io.wrAddr, io.wrData)
  }

  io.rdData := Mux(doForwardReg, wrDataReg, memData)

  if (img.trim().nonEmpty) {
    loadMemoryFromFileInline(mem, img)
  }
}
