package bergamot.interconnect

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.record.StoreQueueBypassIO
import bergamot.core.execute.MemoryAccessLength

import bergamot.bus.SMAReaderIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * SMA interconnect
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** SMA with store queue interconnect
  *
  * The read output port of the Memory executing component, bypassing store queue.
  */
class SMAWithStoreQueueInterconnect extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SMAReaderIO())
    val out = new SMAReaderIO()
    val bypass = new StoreQueueBypassIO()
  })

  io.out.valid := io.in.valid
  io.out.address := io.in.address
  io.out.readType := io.in.readType

  io.in.error := io.out.error
  io.in.ready := io.out.ready

  // Bypass - 32bits
  io.bypass.address := io.in.address
  private val data = Wire(Vec(4, DataType.byte))
  for (i <- 0 until 4) {
    val bypassVal = Mux(io.bypass.strobe(i), io.bypass.data, io.out.data)
    data(i) := extractBits(CoreConstant.byteWidth)(bypassVal, i)
  }

  io.in.data := data(3) ## data(2) ## data(1) ## data(0)
}

/** SMA 2-readers interconnect
  *
  * Priority arbitration, in1 > in2
  */
class SMA2ReaderInterconnect extends Module {
  val io = IO(new Bundle {
    val in1 = Flipped(new SMAReaderIO())
    val in2 = Flipped(new SMAReaderIO())
    val out = new SMAReaderIO()
  })
  private object Status extends ChiselEnum {
    val idle, pending1, pending2 = Value
  }
  private val statusReg = RegInit(Status.idle)

  io.in1.ready := false.B
  io.in1.data := 0.U
  io.in1.error := false.B

  io.in2.ready := false.B
  io.in2.data := 0.U
  io.in2.error := false.B

  io.out.valid := false.B
  io.out.address := 0.U
  io.out.readType := MemoryAccessLength.byte

  when(statusReg === Status.idle) {
    when(io.in1.valid) {
      statusReg := Status.pending1
    }.elsewhen(io.in2.valid) {
      statusReg := Status.pending2
    }
  }

  when(statusReg === Status.pending1) {
    io.out <> io.in1

    when(io.out.ready) {
      statusReg := Status.idle
    }
  }

  when(statusReg === Status.pending2) {
    io.out <> io.in2

    when(io.out.ready) {
      statusReg := Status.idle
    }
  }
}

/** Skip cache SMA reader interconnect
  *
  * Usually let read MMIO with skipping cache
  *
  * @param barrier
  *   Barrier address
  */
class SkipCacheSMAReaderInterconnect(barrier: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new SMAReaderIO())
    val out1 = new SMAReaderIO()
    val out2 = new SMAReaderIO()
  })

  io.in <> new SMAReaderIO().zero
  io.out1 <> new SMAReaderIO().zero
  io.out2 <> new SMAReaderIO().zero

  when(io.in.address < barrier.U) {
    io.out1 <> io.in
  }.otherwise {
    io.out2 <> io.in
  }
}
