package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

class FPConv2FP(width: Int, sign: Boolean) extends Module {
  assert(width == 32 || width == 64)

  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(new FPAddEntry())
  })

  private val ext = Wire(UInt(io.out.significand.getWidth.W))
  ext := io.in

  io.out.exponent := io.out.significand.getWidth.S
  io.out.fpType := FPType.normal
  if (sign) {
    when(io.in.head(1).asBool) { // Negitive
      io.out.significand := ~signExtended(ext, io.in.getWidth - 1) + 1.U
      io.out.sign := 1.U
    }.otherwise { // Positive
      io.out.significand := ext
      io.out.sign := 0.U
    }
  } else {
    io.out.significand := ext
    io.out.sign := 0.U
  }
}

class FPConvFP2(width: Int, sign: Boolean) extends Module {
  assert(width == 32 || width == 64)

  val io = IO(new Bundle {
    val in = Input(new FPAddEntry())
    val out = Output(UInt(width.W))
    val roundoff = Input(FPRoundoff())
    val exception = Output(new FPException())
  })

  io.exception := new FPException().zero

  private val roundingVal = Wire(Vec(width + 1, new FPAddEntry()))
  for (i <- 0 to width) {
    val fillEntry = Wire(new FPAddEntry())
    fillEntry.significand := 0.U((width - i).W) ## io.in.significand.head(fillEntry.significand.getWidth - (width - i))
    fillEntry.exponent := 0.S
    fillEntry.sign := io.in.sign
    fillEntry.fpType := io.in.fpType

    roundingVal(i) := MuxLookup(io.roundoff, fillEntry)(FPRoundoff.roundoffModes(fillEntry, width))
  }

  private val cur = roundingVal(io.in.exponent(log2Ceil(width + 1) - 1, 0))

  if (sign) {
    when(io.in.isNaN()) { // NaN
      io.out := 0.U
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.isPositiveInf()) { // pos-inf
      io.out := Fill(width - 1, 1.U)
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.isNegativeInf()) { // neg-inf
      io.out := 1.U ## Fill(width - 1, 0.U)
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.exponent < 0.S) { // too small
      io.out := 0.U
    }.elsewhen(io.in.exponent > width.S) { // too big
      io.out := Mux(io.in.sign, 1.U ## Fill(width - 1, 0.U), Fill(width - 1, 1.U))
      io.exception.invalidOperation := true.B
    }.elsewhen(cur.exponent === 1.S) { // too big
      io.out := Mux(io.in.sign, 1.U ## Fill(width - 1, 0.U), Fill(width - 1, 1.U))
      io.exception.invalidOperation := true.B
    }.otherwise {
      val signed = cur.significand.head(width)
      when(io.in.sign) {
        when(signed > (1.U ## Fill(width - 1, 0.U))) { // too big
          io.out := 1.U ## Fill(width - 1, 0.U)
          io.exception.invalidOperation := true.B
        }.otherwise {
          io.out := ~signed + 1.U
        }
      }.otherwise {
        when(signed.head(1).asBool) { // too big
          io.out := Fill(width - 1, 1.U)
          io.exception.invalidOperation := true.B
        }.otherwise {
          io.out := signed
        }
      }
    }
  } else {
    when(io.in.isNaN()) { // NaN
      io.out := 0.U
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.isPositiveInf()) { // pos-inf
      io.out := Fill(width, 1.U)
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.sign) { // neg
      io.out := 0.U
      io.exception.invalidOperation := true.B
    }.elsewhen(io.in.exponent < 0.S) { // too small
      io.out := 0.U
    }.elsewhen(io.in.exponent > width.S) { // too big
      io.out := Fill(width, 1.U)
      io.exception.invalidOperation := true.B
    }.elsewhen(cur.exponent === 1.S) { // too big
      io.out := Fill(width, 1.U)
      io.exception.invalidOperation := true.B
    }.otherwise {
      io.out := cur.significand.head(width)
    }
  }
}
