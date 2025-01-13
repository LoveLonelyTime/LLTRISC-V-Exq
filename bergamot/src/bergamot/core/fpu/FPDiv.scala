package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.utils.ChiselUtils._

/*
 * Float-point division unit
 *
 * The minimum implementation of floating-point division using the ready-made divider
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Float-point division unit
  *
  * Accuracy: double + increasement
  */
class FPDiv extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val out = Output(new FPAddEntry())
    val exception = Output(new FPException())
  })

  io.exception := new FPException().zero

  io.out.exponent := io.in1.exponent - io.in2.exponent + io.in1.significand.getWidth.S
  io.out.significand := (io.in1.significand ## Fill(io.in1.significand.getWidth, 0.U) / io.in2.significand) ## 0.U // 64 -> 129
  io.out.sign := io.in1.sign ^ io.in2.sign
  io.out.fpType := FPType.normal

  // Corner case
  when(io.in1.isNaN() || io.in2.isNaN()) { // Quiet NaN
    io.out.fpType := FPType.nan
  }.elsewhen(io.in1.isZero() && io.in2.isZero() || io.in1.isInf() && io.in2.isInf()) { // 0 / 0, inf / inf
    io.out.fpType := FPType.nan
    io.exception.invalidOperation := true.B
  }.elsewhen(io.in1.isZero() || io.in2.isZero()) { // / 0
    io.out.fpType := FPType.inf
    io.exception.divideByZero := true.B
  }.elsewhen(io.in1.isInf()) { // inf /
    io.out.fpType := FPType.inf
  }.elsewhen(io.in2.isInf()) { // / inf
    io.out.sign := 0.U
    io.out.significand := 0.U
  }
}
