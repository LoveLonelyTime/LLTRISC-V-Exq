package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastSlotEntry
import bergamot.core.broadcast.DataBroadcastSendEntry
import bergamot.core.decode.InstructionType

/*
 * Execute entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Execute queue type
  */
object ExecuteQueueType extends ChiselEnum {
  /*
   * none: Discarded instructions
   * memory: Memory access instructions
   * alu: ALU instructions
   * branch: Branch instructions
   * float: FP instructions
   */
  val none, memory, alu, branch, float = Value
}

/** Execute queue enqueue interface
  */
class ExecuteQueueEnqueueIO extends Bundle {
  val enq = DecoupledIO(new ExecuteEntry()) // Data interface
  val queueType = Input(ExecuteQueueType()) // Tell the issue stage the queue type, hardwired
}

/** Execute entry
  *
  * The entry of reservation station, representing an executable instruction
  */
class ExecuteEntry extends Bundle {
  val opcode = DataType.opcode // opcode
  val instructionType = InstructionType() // Instruction Type
  val rs1 = new DataBroadcastSlotEntry() // rs1
  val rs2 = new DataBroadcastSlotEntry() // rs2
  val rs3 = new DataBroadcastSlotEntry() // rs3
  val rd = new DataBroadcastSendEntry() // rd
  val func3 = DataType.func3 // func3
  val func7 = DataType.func7 // func7
  val imm = DataType.immediate // Immediate
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val error = MemoryErrorCode() // Error
  val valid = Bool() // Validity
}

/** Execute result entry
  *
  * The output of execute component
  */
class ExecuteResultEntry extends Bundle {
  // General execution result field, writeback to rd
  val result = DataType.operation

  // Exception field
  val exception = Bool()
  val exceptionCode = DataType.exceptionCode
  val exceptionVal = DataType.operation

  // Write memory field
  val write = Bool()
  val writeID = Vec(2, DataType.receipt)

  // CSR field
  val writeCSR = Bool()
  val csrAddress = DataType.csr
  val csrData = DataType.operation

  // Privilege return field
  val mret = Bool()
  val sret = Bool()

  // Fence field
  val flushDCache = Bool()
  val flushL2DCache = Bool()
  val invalidICache = Bool()
  val invalidTLB = Bool()

  // Predictor field
  val branch = Bool()

  // LRSC field
  val lr = Bool()
  val lrAddress = DataType.address
  val sc = Bool()

  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val real = DataType.address // Real PC
  val valid = Bool() // Validity

  // Helper functions
  def triggerException(code: UInt): Unit = triggerException(code, 0.U)

  def triggerException(code: UInt, value: UInt): Unit = {
    exception := true.B
    exceptionCode := code
    exceptionVal := value
  }

  def resultCSR(addr: UInt, wdata: UInt) = {
    writeCSR := true.B
    csrAddress := addr
    csrData := wdata
  }

  def resultLR(addr: UInt) = {
    lr := true.B
    lrAddress := addr
  }

  def resultMemory(id: Vec[UInt]) = {
    write := true.B
    writeID := id
  }
}
