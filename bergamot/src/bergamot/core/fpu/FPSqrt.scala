package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.utils.ChiselUtils._

/*
 * Float-point sqrt unit
 *
 * The minimum implementation of floating-point division using restoring shift/subtract algorithm
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Float-point sqrt unit
  *
  * Accuracy: double + increasement
  */
class FPSqrt extends Module {
  val io = IO(new Bundle {
    val in = Input(new FPEntry())
    val out = Output(new FPAddEntry())
    val exception = Output(new FPException())
  })

  io.exception := new FPException().zero

  // Restoring shift/subtract algorithm
  private val remainders = Wire(Vec(io.in.significand.getWidth * 2 + 1, UInt((io.in.significand.getWidth * 2 + 2).W)))
  private val roots = Wire(Vec(io.in.significand.getWidth * 2 + 1, UInt((io.in.significand.getWidth * 2).W)))
  remainders(0) := io.in.significand ## Fill(io.in.significand.getWidth, 0.U) // 64 -> 128
  roots(0) := 0.U

  for (i <- 1 to io.in.significand.getWidth * 2) {
    val r2 = remainders(i - 1) ## 0.U // x2
    val term = roots(i - 1).head(i - 1) ## 0.U ## 1.U ## 0.U((io.in.significand.getWidth * 2 - i).W) // x2 + 1
    when(r2 >= term) { // 1
      remainders(i) := r2 - term
      roots(i) := roots(i - 1).head(i - 1) ## 1.U ## 0.U((io.in.significand.getWidth * 2 - i).W)
    }.otherwise { // 0
      remainders(i) := r2
      roots(i) := roots(i - 1).head(i - 1) ## 0.U ## 0.U((io.in.significand.getWidth * 2 - i).W)
    }
  }

  // Exponent by half
  when(io.in.exponent(0)) {
    io.out.exponent := (io.in.exponent + 1.S) >> 1
    io.out.sign := io.in.sign
    io.out.significand := 0.U ## roots(io.in.significand.getWidth * 2) // 128 -> 129
  }.otherwise {
    io.out.exponent := io.in.exponent >> 1
    io.out.sign := io.in.sign
    io.out.significand := roots(io.in.significand.getWidth * 2) ## 0.U // 128 -> 129
  }

  io.out.fpType := FPType.normal

  // Corner case
  when(io.in.isZero()) { // sqrt 0
    io.out.exponent := 0.S
    io.out.significand := 0.U
  }.elsewhen(io.in.sign) { // sqrt < 0
    io.out.fpType := FPType.nan
    io.exception.invalidOperation := true.B
  }.elsewhen(io.in.isInf()) { // sqrt inf
    io.out.fpType := FPType.inf
  }
}
