package bergamot.utils

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastSlotEntry
import bergamot.core.broadcast.DataBroadcastEntry
import bergamot.cache.CacheLineAddress

/*
 * Core utils
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

object CoreUtils {
  val ALIGN_BYTE = 0
  val ALIGN_HALF = 1
  val ALIGN_WORD = 2

  /** Sign extended
    *
    * @param x
    *   UInt
    * @param b
    *   Sign bit
    * @return
    *   width = x.getWidth
    */
  def signExtended(x: UInt, b: Int) = Fill(x.getWidth - b - 1, x(b)) ## x(b, 0)

  /** Align address
    *
    * @param addr
    *   Address
    * @param width
    *   Align width (Alternatives: ALIGN_BYTE, ALIGN_HALF, ALIGN_WORD)
    * @return
    *   Aligned address
    */
  def align(addr: UInt, width: Int) = addr(addr.getWidth - 1, width) ## 0.U(width.W)

  /** Create a pointer from 0 to depth - 1
    * @param depth
    *   Max value = depth - 1
    * @param incr
    *   Increse flag
    * @param init
    *   Init value
    * @return
    *   (current value, next value)
    */
  def pointer(depth: Int, incr: Bool, init: Int = 0) = {
    val cntReg = RegInit(init.U(log2Ceil(depth).W))
    val nextVal = Mux(cntReg === (depth - 1).U, 0.U, cntReg + 1.U)
    when(incr) {
      cntReg := nextVal
    }
    (cntReg, nextVal)
  }

  /** Match a broadcast slot
    *
    * When the match is successful, reset slot pending and replace receipt
    *
    * @param slot
    *   Broadcast slot
    * @param source
    *   Broadcast source
    * @return
    */
  def matchBroadcast(
      output: DataBroadcastSlotEntry,
      slot: DataBroadcastSlotEntry,
      source: DataBroadcastEntry
  ) = {
    when(
      source.valid &&
        slot.pending &&
        slot.receipt === source.receipt
    ) {
      // Reset and replace
      output.pending := false.B
      output.receipt := source.data
    }
  }

  /** Vec(2, data)
    *
    * @param data
    *   Data
    * @return
    *   Vec
    */
  def Vec2[T <: Data](data: T) = Vec(2, data)

  /** VecInit.fill(2)(data)
    *
    * @param data
    *   Data
    * @return
    *   Vec
    */
  def VecInit2[T <: Data](data: T) = VecInit.fill(2)(data)

  /** Is a compress instruction?
    *
    * @param instruction
    *   Instruction
    * @return
    *   result
    */
  def isCompressInstruction(instruction: UInt) = instruction(1, 0) =/= "b11".U

  /** Get word address
    *
    * @param address
    *   Address
    * @return
    *   Word address
    */
  def getWordAddress(address: UInt) = address(CoreConstant.XLEN - 1, 2)

  /** Split cache line address
    *
    * @param cacheLineDepth
    *   Cache line depth
    * @param cacheCellDepth
    *   Cache cell depth
    * @param address
    *   Address to be splited
    * @return
    *   Address case object
    */
  def splitCacheLineAddress(cacheLineDepth: Int, cacheCellDepth: Int)(address: UInt) =
    CacheLineAddress(
      address(CoreConstant.XLEN - 1, log2Ceil(cacheLineDepth) + log2Ceil(cacheCellDepth) + 1),
      address(log2Ceil(cacheLineDepth) + log2Ceil(cacheCellDepth), log2Ceil(cacheCellDepth) + 1),
      address(log2Ceil(cacheCellDepth), 1),
      address(0)
    )

  /** Extract bits
    * @param width
    *   Width
    * @param bits
    *   Bits
    * @param n
    *   Index
    * @return
    *   Bits
    */
  def extractBits[T <: Bits](width: Int)(bits: T, n: Int) = bits(width * n + width - 1, width * n)
}

/** Sv32 helper functions
  */
object Sv32 {
  def getVPN(vaddr: UInt) = vaddr(31, 12)
  def getOffset(vaddr: UInt) = vaddr(11, 0)
  def getMOffset(vaddr: UInt) = vaddr(21, 0)
  def get32PPN(paddr: UInt) = paddr(31, 12)
  def getVPN0(vaddr: UInt) = vaddr(21, 12)
  def getVPN1(vaddr: UInt) = vaddr(31, 22)
}
