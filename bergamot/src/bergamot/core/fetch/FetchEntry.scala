package bergamot.core.fetch

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.decode.DecodeStageEntry
import bergamot.core.execute.MemoryErrorCode

/*
 * Fetch entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Instruction TLB work entry
  *
  * Buffer ITLB table entries
  */
class ITLBWorkEntry extends Bundle {
  val vpn = DataType.vpn // Virtual page number
  val ppn = DataType.ppn20 // Physical page numer
  val error = MemoryErrorCode()
  val valid = Bool()
}

/** Instruction cache line work entry
  *
  * Buffer instruction cache lines
  *
  * @param cacheLineDepth
  *   Instruction cache line depth
  */
class ICacheLineWorkEntry(cacheLineDepth: Int) extends Bundle {
  val content = Vec(cacheLineDepth, DataType.half)
  val address = DataType.address // Aligned cache line address
  val error = MemoryErrorCode()
  val valid = Bool()
}

/** Raw instruction entry
  *
  * An instruction merged
  */
class RawInstructionEntry extends Bundle {
  val instruction = DataType.instruction
  val compress = Bool() // A compression instruction?
  val error = MemoryErrorCode()
  val valid = Bool()
}

/** Speculative entry
  *
  * An instruction predicted
  */
class SpeculativeEntry extends Bundle {
  val instruction = DataType.instruction // Instruction
  val pc = DataType.address // Corresponding PC
  val spec = DataType.address // Speculative PC
  val next = DataType.address // Next PC
  val error = MemoryErrorCode()
  val valid = Bool() // Validity
}
