package bergamot.core.broadcast

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Broadcast entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Data broadcast slot entry
  *
  * A broadcast receiving slot
  */
class DataBroadcastSlotEntry extends Bundle {
  val pending = Bool() // Has it been received?
  // When it has been received, receipt field is data received
  // Otherwise, receipt field is broadcast receipt
  val receipt = DataType.receipt
  val origin = DataType.register // Original register number
}

/** Data broadcast send entry
  *
  * Record rs
  */
class DataBroadcastSendEntry extends Bundle {
  val receipt = DataType.receipt
  val origin = DataType.register // Original register number
}

/** Data broadcast entry
  *
  * A data broadcasts
  */
class DataBroadcastEntry extends Bundle {
  // Broadcast receipt
  val receipt = DataType.receipt
  // Data
  val data = DataType.operation
  val valid = Bool()

  /** Cast a broadcast
    *
    * @param receipt
    *   Data receipt
    * @param data
    *   Data
    */
  def castBroadcast(receipt: UInt, data: UInt) = {
    valid := true.B
    this.receipt := receipt
    this.data := data
  }
}

/** Data broadcast interface
  */
class DataBroadcastIO extends Bundle {
  val entries = Output(Vec2(new DataBroadcastEntry()))
}
