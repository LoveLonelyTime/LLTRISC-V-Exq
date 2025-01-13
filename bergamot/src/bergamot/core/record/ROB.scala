package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.CoreUtils
import bergamot.utils.ChiselUtils._

/*
 * ROB (ReOrder Buffer)
 *
 * The remapped instruction will enter ROB.
 * In ROB, save the status of instructions in the execution core and pipeline.
 * After the instruction is committed, it will retire from the ROB or trigger an event.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ROB components
  *
  * Implementation using read/write pointer queue
  *
  * @param depth
  *   The number of ROB table items, each table entry stores 2 instructions.
  */
class ROB(depth: Int) extends Module {
  require(depth > 0, "ROB table depth must be greater than 0")

  val io = IO(new Bundle {
    // Retire interface
    val retired = DecoupledIO(DataType.receipt)
    /*
     * Alloc interface
     * Return the primary receipt of the table item.
     * The receipts of two table entries are (primary receipt << 1 | 1, primary receipt << 1 | 0)
     */
    val alloc = DecoupledIO(DataType.receipt)
    // ROB table interfaces
    val robTableWrite = Flipped(new ROBTableWriteIO())
    val robTableCommit = Flipped(new ROBTableCommitIO())
    val robTableRetire = new ROBTableRetireIO(depth)
    // Recovery interface
    val recover = Input(Bool())
  })

  // Table logic
  private val table = RegInit(Vec(depth * 2, new ROBTableEntry()).zero)

  // Table write logic
  when(io.robTableWrite.wen) {
    io.robTableWrite.entries.foreach { entry =>
      table(entry.id).pc := entry.pc
      table(entry.id).spec := entry.spec
      table(entry.id).rd := entry.rd
      table(entry.id).valid := entry.valid
      table(entry.id).commit := false.B
    }
  }

  // Table commit logic
  io.robTableCommit.entries.foreach { entry =>
    when(entry.valid) {
      table(entry.rd).executeResult := entry
      table(entry.rd).commit := true.B
    }
  }

  // Table retire logic
  io.robTableRetire.entries := table

  // Queue logic
  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = CoreUtils.pointer(depth, incrRead)
  private val (writePtr, nextWrite) = CoreUtils.pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.alloc.valid := !fullReg
  io.retired.valid := !emptyReg

  io.alloc.bits := writePtr
  io.retired.bits := readPtr

  private val op =
    (io.alloc.valid && io.alloc.ready) ## (io.retired.valid && io.retired.ready)

  switch(op) {
    is("b01".U) { // retired/read
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
    }
    is("b10".U) { // alloc/write
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
    }
    is("b11".U) { // retired-alloc/read-write
      incrRead := true.B
      incrWrite := true.B
    }
  }

  // Recovery logic
  when(io.recover) {
    table.foreach(_.valid := false.B)
  }
}
