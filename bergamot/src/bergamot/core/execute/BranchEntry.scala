package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._

/*
 * Branch entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Branch operation type
  */
object BranchOperationType extends ChiselEnum {
  /*
   * Reserve operation:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Comparison operation:
   * - eq: op1 == op2
   * - ne: op1 != op2
   * - lt: op1 < op2
   * - ge: op1 > op2
   * - ltu: op1 < op2 (unsigned)
   * - geu: op1 > op2 (unsigned)
   *
   * Unconditional operation:
   * - jal: jump and link
   */
  val undefined, none, eq, ne, lt, ge, ltu, geu, jal = Value
}

/** Branch execute stage entry
  *
  * The input entry of BranchExecuteStage
  */
class BranchExecuteStageEntry extends Bundle {
  val op = BranchOperationType() // Branch operation type
  val op1 = DataType.operation32 // Operand 1
  val op2 = DataType.operation32 // Operand 2
  val add1 = DataType.operation32 // Address addend 1
  val add2 = DataType.operation32 // Address addend 2
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
