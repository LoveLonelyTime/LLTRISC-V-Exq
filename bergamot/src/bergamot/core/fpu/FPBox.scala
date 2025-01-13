package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.ChiselUtils._

class FPUnbox(spec: IEEESpec) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(spec.width.W))
    val out = Output(new FPEntry())
    val fpClass = Output(DataType.operation)
  })

  private val sign = io.in.head(1)
  private val exp = io.in.tail(1).head(spec.exponentWidth)
  private val significand = io.in.tail(1 + spec.exponentWidth)

  when(exp.andR) {
    when(significand.orR) { // NaN
      io.out.sign := 0.U
      io.out.significand := 0.U
      io.out.exponent := 0.S
      io.out.fpType := FPType.nan
      io.fpClass := Mux(significand.head(1).asBool, FPClassCode.quietNaN.U, FPClassCode.signalingNaN.U)
    }.otherwise { // Inf
      io.out.sign := sign
      io.out.significand := 0.U
      io.out.exponent := 0.S
      io.out.fpType := FPType.inf
      io.fpClass := Mux(sign.asBool, FPClassCode.negitiveInf.U, FPClassCode.positiveInf.U)
    }
  }.elsewhen(exp === 0.U) { // Subnormal number
    io.out.sign := sign
    io.out.significand := significand ## Fill(io.out.significand.getWidth - significand.getWidth, 0.U)
    io.out.exponent := spec.minExp.S
    io.out.fpType := FPType.normal
    when(significand === 0.U) { // Zero
      io.fpClass := Mux(sign.asBool, FPClassCode.negitiveZero.U, FPClassCode.positiveZero.U)
    }.otherwise {
      io.fpClass := Mux(sign.asBool, FPClassCode.negitiveSubnormal.U, FPClassCode.positiveSubnormal.U)
    }
  }.otherwise { // Normal number
    io.out.sign := sign
    io.out.significand := 1.U ## significand ## Fill(io.out.significand.getWidth - significand.getWidth - 1, 0.U)
    io.out.exponent := exp.asSInt - spec.bias.S + 1.S
    io.out.fpType := FPType.normal
    io.fpClass := Mux(sign.asBool, FPClassCode.negitiveNormal.U, FPClassCode.positiveNormal.U)
  }
}

class FPBox(spec: IEEESpec) extends Module {
  val io = IO(new Bundle {
    val in = Input(new FPAddEntry())
    val out = Output(UInt(spec.width.W))
    val exception = Output(new FPException())
    val roundoff = Input(FPRoundoff())
  })

  io.exception := new FPException().zero

  // Alignment
  private val alignment = WireInit(io.in)

  for (i <- 0 until io.in.significand.getWidth) {
    when(io.in.significand(i)) {
      val filler = alignment.significand.getWidth - i - 1
      alignment.significand := io.in.significand(i, 0) ## 0.U(filler.W)
      alignment.exponent := io.in.exponent - (filler).S
    }
  }

  // Roundoff
  private val subnormalRound = MuxLookup(io.roundoff, alignment)(FPRoundoff.roundoffModes(alignment, spec.significandWidth))
  private val normalRound = MuxLookup(io.roundoff, alignment)(FPRoundoff.roundoffModes(alignment, spec.significandWidth + 1))

  // To IEEE745
  // TODO : Subnormal
  when(io.in.fpType === FPType.inf) { // inf
    io.out := io.in.sign ## Fill(spec.exponentWidth, 1.U) ## Fill(spec.significandWidth, 0.U)
  }.elsewhen(io.in.fpType === FPType.nan) { // NaN
    io.out := io.in.sign ## Fill(spec.exponentWidth, 1.U) ## io.in.significand.head(spec.significandWidth)
  }.elsewhen(subnormalRound.exponent < spec.minExp.S) { // -> 0
    io.out := subnormalRound.sign ## 0.U(spec.exponentWidth.W) ## Fill(spec.significandWidth, 0.U)
  }.elsewhen(subnormalRound.exponent === spec.minExp.S) { // Subnormal number
    io.out := subnormalRound.sign ## 0.U(spec.exponentWidth.W) ## subnormalRound.significand.head(spec.significandWidth)
    io.exception.underflow := true.B
  }.elsewhen(normalRound.exponent > (spec.maxExp + 1).S) { // -> inf
    io.out := normalRound.sign ## Fill(spec.exponentWidth, 1.U) ## Fill(spec.significandWidth, 0.U)
    io.exception.overflow := true.B
  }.otherwise { // Normal number
    val biasExp = Wire(UInt(spec.exponentWidth.W))
    biasExp := (normalRound.exponent + (spec.bias - 1).S).asUInt
    io.out := normalRound.sign ## biasExp ## normalRound.significand.tail(1).head(spec.significandWidth)
  }
}
