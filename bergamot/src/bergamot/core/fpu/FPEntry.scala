package bergamot.core.fpu

import chisel3._
import chisel3.util._

import bergamot.core.DataType
import bergamot.core.CoreConstant

trait IEEESpec {
  val width: Int
  val significandWidth: Int
  val exponentWidth: Int

  def bias = (1 << (exponentWidth - 1)) - 1
  def minExp = -bias + 1
  def maxExp = bias
}

object IEEESpec32 extends IEEESpec {
  val width: Int = 32
  val significandWidth: Int = 23
  val exponentWidth: Int = 8
}

object IEEESpec64 extends IEEESpec {
  val width: Int = 64
  val significandWidth: Int = 52
  val exponentWidth: Int = 11
}

object FPRoundoff extends ChiselEnum {
  val RNE, RTZ, RDN, RUP, RMM = Value

  def roundoffModes(fp: FPAddEntry, reserved: Int) = {
    Seq(
      RNE -> roundTiesToEven(fp, reserved),
      RTZ -> roundTowardZero(fp, reserved),
      RDN -> roundTowardNegative(fp, reserved),
      RUP -> roundTowardPositive(fp, reserved),
      RMM -> roundTiesToMax(fp, reserved)
    )
  }

  /** Increase decimal
    *
    * Remove non reserved bits and increase reserved bits
    * @param fp
    *   Decimal
    * @param reserved
    *   Number of digits to be reserved
    * @return
    *   Result
    */
  def up(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val add = fp.significand.head(reserved) +& 1.U

    result.sign := fp.sign
    result.fpType := fp.fpType
    when(add(reserved)) {
      result.exponent := fp.exponent + 1.S
      result.significand := add(reserved, 1) ## 0.U((result.significand.getWidth - reserved).W)
    }.otherwise {
      result.exponent := fp.exponent
      result.significand := add(reserved - 1, 0) ## 0.U((result.significand.getWidth - reserved).W)
    }

    result
  }

  /** Truncate decimal
    *
    * @param fp
    *   Decimal
    * @param reserved
    *   Number of digits to be reserved
    * @return
    *   Result
    */
  def down(fp: FPAddEntry, reserved: Int) = {
    val result = WireInit(fp)
    result.significand := fp.significand.head(reserved) ## 0.U((result.significand.getWidth - reserved).W)

    result
  }

  def roundTiesToEven(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val judge = fp.significand.head(reserved + 1)(1, 0) ## !fp.significand.tail(reserved + 1).orR
    val upVal = up(fp, reserved)
    val downVal = down(fp, reserved)

    // LSB | Drop MSB | Half
    result := MuxLookup(judge, fp)(
      Seq(
        // Drop MSB is 0
        "b000".U -> downVal,
        "b100".U -> downVal,
        "b001".U -> downVal,
        "b101".U -> downVal,
        // Drop MSB is 1, but not half
        "b010".U -> upVal,
        "b110".U -> upVal,
        // Drop MSB is 1 and half
        "b011".U -> downVal,
        "b111".U -> upVal
      )
    )

    result
  }

  def roundTowardZero(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    result := down(fp, reserved)

    result
  }

  def roundTowardPositive(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val judge = fp.sign ## !fp.significand.tail(reserved).orR
    val upVal = up(fp, reserved)
    val downVal = down(fp, reserved)

    result := MuxLookup(judge, fp)(
      Seq(
        "b00".U -> upVal, // Positive, not zero tail
        "b01".U -> downVal, // Positive, zero tail
        "b10".U -> downVal, // Negitive
        "b11".U -> downVal // Negitive
      )
    )

    result
  }

  def roundTowardNegative(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val judge = fp.sign ## !fp.significand.tail(reserved).orR
    val upVal = up(fp, reserved)
    val downVal = down(fp, reserved)

    result := MuxLookup(judge, fp)(
      Seq(
        "b00".U -> downVal, // Positive
        "b01".U -> downVal, // Positive
        "b10".U -> upVal, // Negitive, not zero tail
        "b11".U -> downVal // Negitive, zero tail
      )
    )

    result
  }

  def roundTiesToMax(fp: FPAddEntry, reserved: Int) = {
    val result = Wire(new FPAddEntry())
    val judge = fp.significand.head(reserved + 1)(1, 0) ## !fp.significand.tail(reserved + 1).orR
    val upVal = up(fp, reserved)
    val downVal = down(fp, reserved)

    // LSB | Drop MSB | Half
    result := MuxLookup(judge, fp)(
      Seq(
        // Drop MSB is 0
        "b000".U -> downVal,
        "b100".U -> downVal,
        "b001".U -> downVal,
        "b101".U -> downVal,
        // Drop MSB is 1, but not half
        "b010".U -> upVal,
        "b110".U -> upVal,
        // Drop MSB is 1 and half
        "b011".U -> upVal,
        "b111".U -> upVal
      )
    )

    result
  }
}

class FPException extends Bundle {
  val invalidOperation = Bool()
  val divideByZero = Bool()
  val overflow = Bool()
  val underflow = Bool()
  val inexact = Bool()
}

object FPType extends ChiselEnum {
  val normal, inf, nan = Value
}

object FPClassCode {
  val negitiveInf = 1
  val negitiveNormal = 2
  val negitiveSubnormal = 4
  val negitiveZero = 8
  val positiveZero = 16
  val positiveSubnormal = 32
  val positiveNormal = 64
  val positiveInf = 128
  val signalingNaN = 256
  val quietNaN = 512
}

trait FP extends Bundle {
  val sign = Bool()
  val significand: UInt
  val exponent: SInt
  val fpType = FPType()

  def isZero() = significand === 0.U
  def isInf() = fpType === FPType.inf
  def isNaN() = fpType === FPType.nan
  def isPositiveInf() = isInf() && !sign
  def isNegativeInf() = isInf() && sign
}

object FP {
  def extend[S <: FP, D <: FP](source: S, dest: D) = {
    assert(source.significand.getWidth <= dest.significand.getWidth)
    dest.fpType := source.fpType
    dest.sign := source.sign
    dest.exponent := source.exponent
    dest.significand := source.significand ## Fill(dest.significand.getWidth - source.significand.getWidth, 0.U)
  }

  def neg[T <: FP](source: T, dest: T) = {
    dest.fpType := source.fpType
    dest.sign := !source.sign
    dest.exponent := source.exponent
    dest.significand := source.significand
  }

  def nan = {
    val result = Wire(new FPEntry())
    result.sign := 0.U
    result.exponent := 0.S
    result.significand := 0.U
    result.fpType := FPType.nan

    result
  }
}

class FPEntry extends FP {
  val significand = DataType.double
  val exponent = SInt(CoreConstant.halfWidth.W)
}

class FPMulEntry extends FP {
  val significand = UInt((DataType.double.getWidth * 2).W)
  val exponent = SInt(CoreConstant.halfWidth.W)
}

class FPAddEntry extends FP {
  val significand = UInt((DataType.double.getWidth * 2 + 1).W)
  val exponent = SInt(CoreConstant.halfWidth.W)
}
