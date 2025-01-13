package bergamot.cache

import chisel3._
import chisel3.util._

import bergamot.core._

/*
 * Cache entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Flush cache interface
  *
  * Request to refresh Cache, there are different implementations for different caches.
  */
class FlushCacheIO extends Bundle {
  val req = Output(Bool()) // Reqeust
  val empty = Input(Bool()) // Empty
}

/** Instruction cache line request interface
  *
  * Request data for an instruction cache line
  *
  * @param cacheCellDepth
  *   Size(bits) = 16 * Cache cell depth
  */
class CacheLineRequestIO(cacheCellDepth: Int) extends Bundle {
  require(cacheCellDepth > 0 && cacheCellDepth % 2 == 0, "Require 32-bit aligned cache cell depth")

  val address = Output(DataType.address) // Cache line address
  val data = Input(Vec(cacheCellDepth, UInt(CoreConstant.cacheCellLength.W))) // Group data
  val error = Input(Bool()) // Memory error
  val valid = Output(Bool())
  val ready = Input(Bool())
}

/** Cache line address
  *
  * Each cache line is composed of multiple cache cells.
  *
  * Considering compatibility with storage of 16 bit compression instructions, each cache unit is 16 bits.
  *
  * @param tag
  *   The remaining portion of the cache address is called Tag, which is used for set associative cache comparison.
  * @param index
  *   Index address of cache line. It is equal to the number of rows in the cache table.
  * @param offset
  *   The address of the cache cell in the cache line is called offset.
  * @param byte
  *   Byte address within cache cell, 1 bit.
  */
case class CacheLineAddress(tag: UInt, index: UInt, offset: UInt, byte: UInt) {

  /** Align address to the cache line
    *
    * @return
    *   Aligned address
    */
  def aligned = tag ## index ## 0.U(offset.getWidth.W) ## 0.U
}
