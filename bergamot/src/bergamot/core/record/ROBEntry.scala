package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.ExecuteResultEntry

import bergamot.utils.CoreUtils._

/*
 * ROB entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ROB table entry
  *
  * A ROB table entry
  */
class ROBTableEntry extends Bundle {
  val executeResult = new ExecuteResultEntry() // Execute result
  val rd = DataType.register // Target rd
  val spec = DataType.address // Speculative PC
  val pc = DataType.address // Blamed PC
  val commit = Bool() // Has been committed?
  val valid = Bool() // Validity
}

/** ROB table write interface
  *
  * ROB registration interface
  */
class ROBTableWriteIO extends Bundle {
  val entries = Output(
    Vec2(
      new Bundle {
        val id = DataType.receipt
        val pc = DataType.address
        val rd = DataType.register
        val spec = DataType.address
        val valid = Bool()
      }
    )
  ) // Registrations
  val wen = Output(Bool()) // Write enable flag
}

/** ROB table commit interface
  *
  * ROB commit interface
  */
class ROBTableCommitIO extends Bundle {
  val entries = Output(Vec(2, new ExecuteResultEntry())) // Committing entries
}

/** ROB table retire interface
  *
  * Used by retire component to query instructions in ROB
  *
  * @param depth
  *   The number of ROB table items, each table entry stores 2 instructions.
  */
class ROBTableRetireIO(depth: Int) extends Bundle {
  val entries = Output(Vec(depth * 2, new ROBTableEntry()))
}
