package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._

/*
 * ALU entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ALU operation type
  */
object ALUOperationType extends ChiselEnum {
  /*
   * Reserve operations:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Arithmetic operations:
   * - add: op1 + op2
   * - sub: op1 - op2
   *
   * Logical operations:
   * - and: op1 & op2
   * - or: op1 | op2
   * - xor: op1 ^ op2
   *
   * Shift operations:
   * - sll: op1 << op2
   * - srl: op1 >> op2 (zero-extend)
   * - sra: op1 >> op2 (sign-extend)
   *
   * Compare then set operations:
   * - slt: if op1 < op1 then 1 else 0
   * - sltu: if op1 < op1 (unsigned) then 1 else 0
   *
   * CSR operations:
   * - csrrw: CSR read then write
   * - csrrs: CSR read then set
   * - csrrc: CSR read then clear
   *
   * Privilege switch operations:
   * - env: Trigger environment call trap (exception)
   * - ebreak: Environment breakpoint (exception)
   * - mret: M-Level Environment call return
   * - sret: M-Level Environment call return
   *
   * Fence operations:
   * - fence: Memory fence
   * - fencei: Instruction memory fence
   * - sfenceVMA: Update TLB
   *
   * Multiplication and division operations:
   * - mul, mulh, mulhsu, mulhu: Multiplication
   * - div, divu: Division
   * - rem, remu: Modulus
   */
  val undefined, none, add, sub, and, or, xor, sll, srl, sra, slt, sltu, csrrw, csrrs, csrrc, env, ebreak, mret, sret, fence, fencei, sfenceVMA, mul, mulh, mulhsu, mulhu, div, divu, rem, remu = Value
}

/** ALU execute stage entry
  *
  * The input entry of ALUExecuteStage
  */
class ALUExecuteStageEntry extends Bundle {
  val op = ALUOperationType() // ALU operation type
  val op1 = DataType.operation32 // Operand 1
  val op2 = DataType.operation32 // Operand 2
  val rd = DataType.receipt // Destination receipt
  val csrAddress = DataType.csr // CSR address
  val error = MemoryErrorCode() // Error
  val csrError = Bool() // CSR error
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}
