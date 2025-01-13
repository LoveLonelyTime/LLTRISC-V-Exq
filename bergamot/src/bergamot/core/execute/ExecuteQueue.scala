package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core.broadcast.DataBroadcastIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Execute queue (aka reservation station)
 *
 * Instructions are temporarily stored in the reservation station, waiting for wake-up(Data Capture).
 * Instructions listening data broadcast in the reservation station.
 * Afterwards, fire and execute instructions according to different wake-up algorithms.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** The abstract class ExecuteQueue
  *
  * Describe the basic interface
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  * @param enableRs3
  *   Is rs3 listening enabled (for FPU)
  */
abstract class ExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type, enableRs3: Boolean) extends Module {
  require(depth > 0, "Execute queue depth must be greater than 0")

  val io = IO(new Bundle {
    // Enq and deq interface
    val enqAndType = Flipped(new ExecuteQueueEnqueueIO())
    val deq = DecoupledIO(new ExecuteEntry())
    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Hardwired
  io.enqAndType.queueType := queueType
}

/** The in ordered implementation of execute queue
  *
  * This execute queue is in ordered, which is suitable for instruction sets that require in ordered within the queue.
  *
  * Implementation using read/write pointer queue
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  * @param enableRs3
  *   Is rs3 listening enabled (for FPU)
  */
class InOrderedExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type, enableRs3: Boolean) extends ExecuteQueue(depth, queueType, enableRs3) {

  // Read/Wirte pointers
  private val queue = RegInit(Vec(depth, new ExecuteEntry()).zero)

  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = pointer(depth, incrRead)
  private val (writePtr, nextWrite) = pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.enqAndType.enq.ready := !fullReg

  // In ordered arbitration logic
  io.deq.valid := !emptyReg && // Not empty
    (
      (!queue(readPtr).rs1.pending &&
        !queue(readPtr).rs2.pending &&
        (if (enableRs3) !queue(readPtr).rs3.pending else true.B)) || // Operands are ready
        !queue(readPtr).valid // Or invalid
    )

  io.deq.bits := queue(readPtr)

  // Queue logic
  private val op = (io.enqAndType.enq.valid && io.enqAndType.enq.ready) ##
    (io.deq.valid && io.deq.ready)
  private val doWrite = WireDefault(false.B)

  switch(op) {
    is("b01".U) { // read
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
    }
    is("b10".U) { // write
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
      doWrite := true.B
    }

    is("b11".U) { // read and write
      incrRead := true.B
      incrWrite := true.B
      doWrite := true.B
    }
  }

  // Broadcast logic
  for (
    entry <- queue;
    broadcast <- io.broadcast.entries
  ) {
    matchBroadcast(entry.rs1, entry.rs1, broadcast)
    matchBroadcast(entry.rs2, entry.rs2, broadcast)
    if (enableRs3)
      matchBroadcast(entry.rs3, entry.rs3, broadcast)
  }

  // Write logic
  when(doWrite) {
    queue(writePtr) := io.enqAndType.enq.bits
  }

  // Recovery logic
  when(io.recover) {
    queue.foreach(_.valid := false.B)
  }
}

/** The out of ordered implementation of execute queue
  *
  * Arbitration strategy is from old to new
  *
  * Implementation using double buffer queue
  *
  * @param depth
  *   Queue depth
  * @param queueType
  *   Queue type
  * @param enableRs3
  *   Is rs3 listening enabled (for FPU)
  */
class OutOfOrderedExecuteQueue(depth: Int, queueType: ExecuteQueueType.Type, enableRs3: Boolean) extends ExecuteQueue(depth, queueType, enableRs3) {

  /** Double buffer cell
    */
  private class DoubleBuffer extends Module {
    val io = IO(new Bundle {
      val enq = Flipped(DecoupledIO(new ExecuteEntry()))
      val deq = DecoupledIO(new ExecuteEntry())
      val sideDeq = DecoupledIO(new ExecuteEntry()) // Firing path
      val broadcast = Flipped(new DataBroadcastIO())
      val recover = Input(Bool())
    })

    private object State extends ChiselEnum {
      val empty, one, two = Value
    }

    private val stateReg = RegInit(State.empty)
    private val dataReg = Reg(new ExecuteEntry())
    private val shadowReg = Reg(new ExecuteEntry())

    private val deqFire = io.deq.fire || io.sideDeq.fire

    io.enq.ready := (stateReg === State.empty || stateReg === State.one)
    io.sideDeq.valid := (stateReg === State.one || stateReg === State.two) &&
      ((!dataReg.rs1.pending && // Operands are ready
        !dataReg.rs2.pending &&
        (if (enableRs3) !dataReg.rs3.pending else true.B)) || // Or invalid
        !dataReg.valid)

    io.deq.valid := (stateReg === State.one || stateReg === State.two) && !io.sideDeq.valid

    io.deq.bits := dataReg
    io.sideDeq.bits := dataReg

    // Broadcast logic
    io.broadcast.entries.foreach { broadcast =>
      // Bypass
      matchBroadcast(io.deq.bits.rs1, dataReg.rs1, broadcast)
      matchBroadcast(io.deq.bits.rs2, dataReg.rs2, broadcast)
      if (enableRs3)
        matchBroadcast(io.deq.bits.rs3, dataReg.rs3, broadcast)
      // Listen
      matchBroadcast(dataReg.rs1, dataReg.rs1, broadcast)
      matchBroadcast(shadowReg.rs1, shadowReg.rs1, broadcast)
      matchBroadcast(dataReg.rs2, dataReg.rs2, broadcast)
      matchBroadcast(shadowReg.rs2, shadowReg.rs2, broadcast)
      if (enableRs3) {
        matchBroadcast(dataReg.rs3, dataReg.rs3, broadcast)
        matchBroadcast(shadowReg.rs3, shadowReg.rs3, broadcast)
      }
    }

    // Double buffer write logic
    switch(stateReg) {
      is(State.empty) {
        when(io.enq.valid) {
          stateReg := State.one
          dataReg := io.enq.bits
        }
      }

      is(State.one) {
        when(deqFire && !io.enq.valid) {
          stateReg := State.empty
        }
        when(deqFire && io.enq.valid) {
          stateReg := State.one
          dataReg := io.enq.bits
        }
        when(!deqFire && io.enq.valid) {
          stateReg := State.two
          shadowReg := io.enq.bits
        }
      }

      is(State.two) {
        when(deqFire) {
          dataReg := shadowReg
          io.broadcast.entries.foreach { broadcast => // Bypass
            matchBroadcast(dataReg.rs1, shadowReg.rs1, broadcast)
            matchBroadcast(dataReg.rs2, shadowReg.rs2, broadcast)
            if (enableRs3)
              matchBroadcast(dataReg.rs3, shadowReg.rs3, broadcast)
          }
          stateReg := State.one
        }
      }
    }

    // Recovery logic
    when(io.recover) {
      dataReg.valid := false.B
      shadowReg.valid := false.B
    }
  }

  private val buffers = Array.fill(depth)(Module(new DoubleBuffer()))
  buffers.foreach { cell =>
    cell.io.broadcast := io.broadcast
    cell.io.recover := io.recover
  }

  // Buffer chain
  io.enqAndType.enq <> buffers(0).io.enq
  for (i <- 0 until (depth - 1)) {
    buffers(i).io.deq <> buffers(i + 1).io.enq
  }
  buffers(depth - 1).io.deq.ready := false.B // Block tail

  // Arbitration, from tail to head (old to new)
  // The earlier the instruction is executed, the more instructions can be awakened
  val grant = VecInit.fill(depth)(false.B)
  val notGranted = VecInit.fill(depth)(false.B)
  grant(depth - 1) := buffers(depth - 1).io.sideDeq.valid
  notGranted(depth - 1) := !grant(depth - 1)

  for (i <- (0 to (depth - 2)).reverse) {
    grant(i) := buffers(i).io.sideDeq.valid && notGranted(i + 1)
    notGranted(i) := !grant(i) && notGranted(i + 1)
  }

  buffers.foreach(_.io.sideDeq.ready := false.B)
  io.deq.valid := false.B
  io.deq.bits := new ExecuteEntry().zero
  for (i <- 0 until depth) {
    when(grant(i)) {
      buffers(i).io.sideDeq <> io.deq
    }
  }
}
