package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.MemoryErrorCode

/*
 * TLB entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** TLB entry
  */
class TLBEntry extends Bundle {
  val vpn = DataType.vpn // Virtual page number
  val ppn = DataType.ppn22 // Physical page numer
  val asid = DataType.asid // Address space ID
  val uxwr = UInt(4.W) // User/Execute/Write/Read
  val da = UInt(2.W) // Dirty/Access
  val g = Bool() // Global
  val v = Bool() // Valid
  val mPage = Bool() // MiB page
  val valid = Bool() // Entry valid
}

/** TLB request interface
  */
class TLBRequestIO extends Bundle {
  val vaddress = Output(DataType.address) // Virtual address
  val write = Output(Bool()) // Store operation
  val paddress = Input(DataType.address) // Physical address
  val error = Input(MemoryErrorCode()) // Error
  val valid = Output(Bool())
  val ready = Input(Bool())
}
