package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.fpu.FPEntry
import bergamot.core.fpu.FPMulEntry
import bergamot.core.fpu.FPAddEntry
import bergamot.core.fpu.FPException

object FPUOperationType extends ChiselEnum {
  /*
   * Reserve operations:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Fused arithmetic operations:
   * - madd: op1 * op2 + op3
   * - msub: op1 * op2 - op3
   * - nmsub: -op1 * op2 + op3
   * - nmadd: -op1 * op2 - op3
   *
   * Arithmetic operations:
   * - add: op1 + op2
   * - sub: op1 - op2
   * - mul: op1 * op2
   * - div: op1 / op2
   * - sqrt: sqrt(op1)
   *
   * Sign operations:
   * - sgnj: op2 using sign of op1
   * - sgnjn: op2 using sign of -op1
   * - sgnjx: op2 using sign of op1 ^ op2
   *
   * Comparison operations:
   * - min: min(op1,op2)
   * - max: max(op1,op2)
   * - eq: op1 == op2
   * - le: op1 <= op2
   * - lt: op1 < op2
   *
   * Convertion operations:
   * - cvtff: FP to FP conversion
   * - cvtfx: Integer to FP conversion
   * - cvtxf: FP to integer conversion
   *
   * Move operations:
   * - mvfx: x-registers to f-registers
   * - mvxf: f-registers to x-registers
   */
  val undefined, none, madd, msub, nmsub, nmadd, add, sub, mul, div, sqrt, sgnj, sgnjn, sgnjx, min, max, eq, lt, le, cvtff, cvtfx, cvtxf, clazz, mvfx, mvxf = Value
}

object FPUOperationWidth extends ChiselEnum {
  val undefined, float, double = Value

  def identify(data: UInt) = {
    MuxLookup(data, FPUOperationWidth.undefined)(
      Seq(
        "b00".U -> FPUOperationWidth.float, // Single-precision floating-point number
        "b01".U -> FPUOperationWidth.double // Double-precision floating-point number
      )
    )
  }
}

class FPOperand extends Bundle {
  val raw = DataType.operation
  val fpClass = DataType.operation
  val entry = new FPEntry()
}

class FPUMulStageEntry extends Bundle {
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
  val fpOp1 = new FPOperand() // FP operand 1
  val fpOp2 = new FPOperand() // FP operand 2
  val fpOp3 = new FPOperand() // FP operand 3
  val xOp = DataType.operation // Integer operand
  val signed = Bool() // Signed
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class FPUAddStageEntry extends Bundle {
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
  val fpOp1 = new FPMulEntry() // FP operand 1
  val fpOp2 = new FPMulEntry() // FP operand 2
  val fpRes = new FPAddEntry() // FP lead result
  val xOp = DataType.operation // Integer operand
  val signed = Bool() // Signed
  val exception = new FPException() // FP exception
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

class FPUBoxStageEntry extends Bundle {
  val op = FPUOperationType() // FPU operation type
  val fpWidth = FPUOperationWidth() // FPU operation width
  val fpRm = DataType.func3 // Instruction rounding mode
  val fpOp = new FPAddEntry() // FP operand
  val xOp = DataType.operation // Integer operand
  val signed = Bool() // Signed
  val exception = new FPException() // FP exception
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
