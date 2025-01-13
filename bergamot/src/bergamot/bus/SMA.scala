package bergamot.bus

import chisel3._
import chisel3.util._

import bergamot.core.execute.MemoryAccessLength

/*
 * SMA (Simple Memory Access) bus interface
 *
 * SMA is a simple storage (cache, memory or IO) access bus interface that provides byte, half word, and single word access.
 * SMA provides a simpler interface to access storage devices, and sender does not need to consider storage properties.
 * The sender request bus by setting valid signal, then keep the request parameters unchanged.
 * The storage device will transmit result through a cycle of ready signal.
 * Please note that once the request is started, it cannot be cancelled until the ready signal is high.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** SMA specification
  */
object SMASpec {
  val addressWidth = 32
  val dataWidth = 32
}

/** SMAReaderIO
  *
  * SMA reader interface
  */
class SMAReaderIO extends Bundle {
  // Out
  val address = Output(UInt(SMASpec.addressWidth.W))
  val readType = Output(MemoryAccessLength())
  // In
  val data = Input(UInt(SMASpec.dataWidth.W))
  val error = Input(Bool())
  // Handshake signals
  val valid = Output(Bool())
  val ready = Input(Bool())
}

/** SMAWriterIO
  *
  * SMA writer interface
  */
class SMAWriterIO extends Bundle {
  // Out
  val address = Output(UInt(SMASpec.addressWidth.W))
  val data = Output(UInt(SMASpec.dataWidth.W))
  val writeType = Output(MemoryAccessLength())
  // In
  val error = Input(Bool())
  // Handshake signals
  val valid = Output(Bool())
  val ready = Input(Bool())
}
