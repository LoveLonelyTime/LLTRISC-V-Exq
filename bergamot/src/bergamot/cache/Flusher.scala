package bergamot.cache

import chisel3._
import chisel3.util._

/*
 * Flusher
 *
 * Provide flusher implementations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Serial 2-ways flusher
  *
  * When requesting a flush, flush `out1` first and then flush `out2`.
  */
class Serial2Flusher extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new FlushCacheIO())
    val out1 = new FlushCacheIO()
    val out2 = new FlushCacheIO()
  })

  private object Status extends ChiselEnum {
    val idle, flush1, flush2 = Value
  }

  private val statusReg = RegInit(Status.idle)

  private val empty = io.out1.empty && io.out2.empty

  io.in.empty := empty && statusReg === Status.idle

  io.out1.req := false.B
  io.out2.req := false.B

  when(statusReg === Status.idle && io.in.req && !empty) {
    statusReg := Status.flush1
  }

  when(statusReg === Status.flush1) {
    io.out1.req := true.B
    when(empty) {
      statusReg := Status.idle
    }.elsewhen(io.out1.empty) {
      statusReg := Status.flush2
    }
  }

  when(statusReg === Status.flush2) {
    io.out2.req := true.B
    when(empty) {
      statusReg := Status.idle
    }
  }
}

/** Parallel 2-ways flusher
  *
  * When requesting a flush, flush `out1` and `out2` simultaneously.
  */
class Parallel2Flusher extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new FlushCacheIO())
    val out1 = new FlushCacheIO()
    val out2 = new FlushCacheIO()
  })

  io.out1.req := io.in.req
  io.out2.req := io.in.req
  io.in.empty := io.out1.empty && io.out2.empty
}
