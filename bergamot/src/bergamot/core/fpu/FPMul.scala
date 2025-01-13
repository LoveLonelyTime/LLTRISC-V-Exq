package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.utils.ChiselUtils._

/*
 * Float-point multiplication unit
 *
 * The minimum implementation of floating-point multiplication using the ready-made multiplier
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Float-point multiplication unit
  *
  * Accuracy: double
  */
class FPMul extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val out = Output(new FPMulEntry())
    val exception = Output(new FPException())
  })

  io.exception := new FPException().zero

  io.out.significand := io.in1.significand * io.in2.significand // 64 -> 128
  io.out.exponent := io.in1.exponent + io.in2.exponent
  io.out.sign := io.in1.sign ^ io.in2.sign
  io.out.fpType := FPType.normal

  // Corner case
  when(io.in1.isNaN() || io.in2.isNaN()) { // Quiet NaN
    io.out.fpType := FPType.nan
  }.elsewhen(io.in1.isInf() && io.in2.isZero() || io.in2.isInf() && io.in1.isZero()) { // inf * 0
    io.out.fpType := FPType.nan
    io.exception.invalidOperation := true.B
  }.elsewhen(io.in1.isInf() || io.in2.isInf()) { // * inf
    io.out.fpType := FPType.inf
  }
}
