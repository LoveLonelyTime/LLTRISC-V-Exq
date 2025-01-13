package bergamot.interconnect

import chisel3._
import chisel3.util._

import bergamot.cache.CacheLineRequestIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Cache interconnect
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Cache line 2-requestors interconnect
  *
  * Priority arbitration, in1 > in2
  *
  * @param cacheCellDepth
  *   Cache cell depth
  */
class CacheLineRequest2Interconnect(cacheCellDepth: Int) extends Module {
  require(cacheCellDepth > 0 && cacheCellDepth % 2 == 0, "Require 32-bit aligned cache cell depth")

  val io = IO(new Bundle {
    val in1 = Flipped(new CacheLineRequestIO(cacheCellDepth))
    val in2 = Flipped(new CacheLineRequestIO(cacheCellDepth))
    val out = new CacheLineRequestIO(cacheCellDepth)
  })

  private object Status extends ChiselEnum {
    val idle, pending1, pending2 = Value
  }
  private val statusReg = RegInit(Status.idle)

  io.in1 <> new CacheLineRequestIO(cacheCellDepth).zero
  io.in2 <> new CacheLineRequestIO(cacheCellDepth).zero
  io.out <> new CacheLineRequestIO(cacheCellDepth).zero

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
