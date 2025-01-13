package bergamot.core.fpu

import chisel3._
import chisel3.util._

/*
 * Float-point comparison unit
 *
 * The minimum implementation of floating-point comparison
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Float-point comparison unit
  */
class FPComp extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())

    val eq = Output(Bool())
    val lt = Output(Bool())
  })

  when(io.in1.isNaN() || io.in2.isNaN()) { // NaN
    io.eq := false.B
  }.elsewhen(io.in1.isInf() || io.in2.isInf()) { // inf
    io.eq := false.B
  }.otherwise {
    io.eq := io.in1.sign === io.in2.sign && io.in1.exponent === io.in2.exponent && io.in1.significand === io.in2.significand
  }

  io.lt := false.B
  when(io.in1.isNaN() || io.in2.isNaN()) { // NaN
    io.lt := false.B
  }.elsewhen(io.in1.isInf() || io.in2.isInf()) { // inf
    io.lt := !io.in1.isPositiveInf() && io.in2.isPositiveInf() || io.in1.isNegativeInf() && !io.in2.isNegativeInf()
  }.elsewhen(io.in1.sign && !io.in2.sign) { // neg < pos
    io.lt := true.B
  }.elsewhen(!io.in1.sign && !io.in2.sign) { // Both pos
    io.lt := io.in1.exponent < io.in2.exponent || io.in1.exponent === io.in2.exponent && io.in1.significand < io.in2.significand
  }.elsewhen(io.in1.sign && io.in2.sign) { // Both neg
    io.lt := io.in1.exponent > io.in2.exponent || io.in1.exponent === io.in2.exponent && io.in1.significand > io.in2.significand
  }
}
