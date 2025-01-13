package bergamot.core

import chisel3._

/*
 * Data type declarations
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Basic data type.
  */
object DataType {
  /* Basic types */
  def byte = UInt(CoreConstant.byteWidth.W) // byte
  def half = UInt(CoreConstant.halfWidth.W) // half word
  def word = UInt(CoreConstant.wordWidth.W) // word
  def double = UInt(CoreConstant.doubleWidth.W) // double word
  def quad = UInt(CoreConstant.quadWidth.W) // quad word

  /* Architecture types */
  def address = word // address
  def opcode = UInt(7.W) // 7-bits opcode
  def instruction = word // instruction
  def register = UInt(6.W) // 6-bits register(combined with f registers) id
  def register5 = UInt(5.W) // 5-bits register id
  def func3 = UInt(3.W) // 3-bits func3
  def func7 = UInt(7.W) // 7-bits func7
  def immediate = word // raw immediate
  def zimmediate = UInt(5.W) // 5-bits CSR zero-extend immediate
  def receipt = double // broadcast receipt
  def operation32 = word // ! Compromise from 64 bit to 32 bit !
  def operation = double // operand
  def csr = UInt(12.W) // 12-bits CSR address
  def exceptionCode = UInt(5.W) // 4-bits exception code
  def strobe = UInt(4.W) // 4-bits for 32-bits(4-lanes) strobe
  def asid = UInt(9.W) // 9-bits address space ID
  def vpn = UInt(20.W) // Virtual page number
  def ppn20 = UInt(20.W) // 20-bits physical page number
  def ppn22 = UInt(22.W) // 22-bits physical page number
}

object CoreConstant {
  val byteWidth = 8
  val halfWidth = 16
  val wordWidth = 32
  val doubleWidth = 64
  val quadWidth = 128

  val XLEN = 32
  val instructionLength = 4
  val compressInstructionLength = 2
  val cacheCellLength = 16
}
