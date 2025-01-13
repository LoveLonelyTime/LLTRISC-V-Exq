package bergamot.core.broadcast

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.record.ROBTableCommitIO
import bergamot.core.execute.ExecuteResultEntry

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Broadcaster
 *
 * This core adopts a data broadcast/reception method for data forward.
 * The data required by an instruction is received by waiting for broadcast (aka pending).
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Abstract broadcaster
  *
  * Define a set of universal interfaces
  *
  * A broadcaster selects instructions waiting to be committed from all execution queues through arbitration algorithm. The result of these instructions will be broadcasted through data broadcasting and written back to ROB.
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
abstract class Broadcaster(executeQueueWidth: Int) extends Module {
  require(executeQueueWidth > 0, "Execute queue depth must be greater than 0")

  val io = IO(new Bundle {
    // Execute queue interfaces
    val queues = Vec(executeQueueWidth, Flipped(DecoupledIO(new ExecuteResultEntry())))
    // Broadcast interface
    val broadcast = new DataBroadcastIO()
    // ROB table commit interface
    val robTableCommit = new ROBTableCommitIO()
  })
}

/** Round-robin broadcaster
  *
  * Round-robin (RR) arbitration algorithm
  *
  * Performance: to be profiled
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class RoundRobinBroadcaster(executeQueueWidth: Int) extends Broadcaster(executeQueueWidth) {
  // Single cycle auto increasing loop pointer
  private val (focusPointer, nextFocusPointer) = pointer(executeQueueWidth, true.B)

  /** Commit queue to entry
    *
    * @param queuePtr
    *   Pointer of queue
    * @param entryPtr
    *   Pointer of entry
    */
  def commit(queuePtr: UInt, entryPtr: Int) = {
    when(io.queues(queuePtr).valid) {
      io.queues(queuePtr).ready := true.B

      // Write back the result
      io.robTableCommit.entries(entryPtr) := io.queues(queuePtr).bits

      // Broadcast
      when(io.queues(queuePtr).bits.valid) {
        io.broadcast.entries(entryPtr).castBroadcast(io.queues(queuePtr).bits.rd, io.queues(queuePtr).bits.result)
      }
    }
  }

  // Commit logic
  io.queues.foreach(_.ready := false.B)
  io.broadcast <> new DataBroadcastIO().zero
  io.robTableCommit <> new ROBTableCommitIO().zero

  // Commit 2 results in sequence
  commit(focusPointer, 0)
  commit(nextFocusPointer, 1)

  // Bubble elimination
  io.queues.foreach { entry =>
    when(!entry.bits.valid) {
      entry.ready := true.B
    }
  }
}
