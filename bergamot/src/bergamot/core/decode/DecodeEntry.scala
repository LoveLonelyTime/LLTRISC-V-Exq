package bergamot.core.decode

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastSlotEntry
import bergamot.core.broadcast.DataBroadcastSendEntry
import bergamot.core.execute.MemoryErrorCode
import bergamot.core.execute.ExecuteQueueType

/*
 * Decode entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** RISC-V instruction types
  *
  * Although RISC-V has 6 types of instructions, each type has various variants.
  */
object InstructionType extends ChiselEnum {
  /*
   * UNK: Unknown (Illegal instruction exception)
   * R: Register
   * R4: Register 4
   * I: Immediate
   * S: Store
   * B: Branch
   * U: Upper
   * J: Jump
   */
  val UNK, R, R4, I, S, B, U, J = Value
}

/** Decode stage entry
  *
  * The input of decode stage
  */
class DecodeStageEntry extends Bundle {
  val instruction = DataType.instruction // 32-bits raw instruction
  val pc = DataType.address // Corresponding PC
  val spec = DataType.address // Speculative PC
  val next = DataType.address // Next PC
  val error = MemoryErrorCode() // Error
  val valid = Bool() // Validity
}

class RegisterEntry extends Bundle {
  val reg = DataType.register
  val mapping = Bool()
}

/** Register mapping stage entry
  *
  * The input of register mapping stage
  */
class RegisterMappingStageEntry extends Bundle {
  val opcode = DataType.opcode // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = new RegisterEntry() // rs1
  val rs2 = new RegisterEntry() // rs2
  val rs3 = new RegisterEntry() // rs3
  val rd = new RegisterEntry() // rd
  val func3 = DataType.func3 // func3
  val func7 = DataType.func7 // func7
  val imm = DataType.immediate // Immediate
  val pc = DataType.address // Corresponding PC
  val spec = DataType.address // Speculative PC
  val next = DataType.address // Next PC
  val error = MemoryErrorCode() // Error
  val valid = Bool() // Validity
}

/** Issue stage entry
  *
  * The input of issue stage
  */
class IssueStageEntry extends Bundle {
  val opcode = DataType.opcode // opcode
  val instructionType = InstructionType() // Instruction Type
  val executeQueue = ExecuteQueueType() // Execute queue
  val rs1 = new DataBroadcastSlotEntry() // Broadcast rs1
  val rs2 = new DataBroadcastSlotEntry() // Broadcast rs2
  val rs3 = new DataBroadcastSlotEntry() // Broadcast rs2
  val rd = new DataBroadcastSendEntry() // rd
  val func3 = DataType.func3 // func3
  val func7 = DataType.func7 // func7
  val imm = DataType.immediate // Immediate
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val error = MemoryErrorCode() // Error
  val valid = Bool() // Validity
}
