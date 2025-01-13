package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.MemoryAccessLength

import bergamot.bus.SMAWriterIO

import bergamot.cache.FlushCacheIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Store queue
 *
 * The store queue is responsible for buffering memory write instructions.
 * Provide recovery of memory write instructions.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Store queue
  *
  * Implemented by loop pointers
  *
  * @param depth
  *   Store queue depth
  */
class StoreQueue(depth: Int) extends Module {
  val io = IO(new Bundle {
    // Alloc interface
    val alloc = Flipped(new StoreQueueAllocIO())
    // Dequeue interface
    val deq = DecoupledIO(new StoreQueueDequeueEntry())
    // Retire interface
    val retire = Flipped(new StoreQueueRetireIO())
    // Bypass interface
    val bypass = Flipped(new StoreQueueBypassIO())
    // Flush request interface: Dequeue all retired
    val flush = Flipped(new FlushCacheIO())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Read/Wirte pointers
  private val queue = RegInit(Vec(depth, new StoreQueueEntry()).zero)

  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = pointer(depth, incrRead)
  private val (writePtr, nextWrite) = pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.alloc.ready := !fullReg && !io.recover
  io.alloc.id := writePtr

  // In ordered arbitration logic
  io.deq.valid := !emptyReg && // Not empty
    (
      queue(readPtr).retire || // Retired
        !queue(readPtr).valid // Or invalid
    )

  io.deq.bits.writeType := queue(readPtr).writeType
  io.deq.bits.data := queue(readPtr).data
  io.deq.bits.address := queue(readPtr).address
  io.deq.bits.valid := queue(readPtr).valid

  // Queue logic
  private val op = (io.alloc.valid && io.alloc.ready) ## io.deq.fire
  private val doWrite = WireDefault(false.B)
  private val doRead = WireDefault(false.B)
  switch(op) {
    is("b01".U) { // dequeue
      fullReg := false.B
      emptyReg := nextRead === writePtr
      incrRead := true.B
      doRead := true.B
    }
    is("b10".U) { // alloc
      emptyReg := false.B
      fullReg := nextWrite === readPtr
      incrWrite := true.B
      doWrite := true.B
    }
    is("b11".U) { // alloc-dequeue
      incrRead := true.B
      incrWrite := true.B
      doWrite := true.B
      doRead := true.B
    }
  }

  // Write logic
  when(doWrite) {
    queue(writePtr).writeType := io.alloc.writeType
    queue(writePtr).address := io.alloc.address
    queue(writePtr).data := io.alloc.data
    queue(writePtr).valid := true.B
    queue(writePtr).retire := false.B
  }

  // Read logic
  when(doRead) {
    queue(readPtr).valid := false.B
  }

  // Bypass logic
  private val laneStrobes = VecInit.fill(4)(false.B)
  private val laneData = VecInit.fill(4)(DataType.byte.zeroAsUInt)

  // TODO: I want to optimize this code snippet
  // Overlapping window
  private def bypassEntry(id: Int) = {
    when(queue(id).valid) {
      switch(queue(id).writeType) {
        is(MemoryAccessLength.byte) {
          when(queue(id).address === io.bypass.address) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(7, 0)
          }.elsewhen(queue(id).address === io.bypass.address + 1.U) {
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(7, 0)
          }.elsewhen(queue(id).address === io.bypass.address + 2.U) {
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(7, 0)
          }.elsewhen(queue(id).address === io.bypass.address + 3.U) {
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(7, 0)
          }
        }

        is(MemoryAccessLength.half) {
          when(queue(id).address === io.bypass.address - 1.U) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(15, 8)
          }.elsewhen(queue(id).address === io.bypass.address) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(7, 0)
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(15, 8)
          }.elsewhen(queue(id).address === io.bypass.address + 1.U) {
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(7, 0)
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(15, 8)
          }.elsewhen(queue(id).address === io.bypass.address + 2.U) {
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(7, 0)
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(15, 8)
          }.elsewhen(queue(id).address === io.bypass.address + 3.U) {
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(7, 0)
          }
        }

        is(MemoryAccessLength.word) {
          when(queue(id).address === io.bypass.address - 3.U) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(31, 24)
          }.elsewhen(queue(id).address === io.bypass.address - 2.U) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(23, 16)
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(31, 24)
          }.elsewhen(queue(id).address === io.bypass.address - 1.U) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(15, 8)
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(23, 16)
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(31, 24)
          }.elsewhen(queue(id).address === io.bypass.address) {
            laneStrobes(0) := true.B
            laneData(0) := queue(id).data(7, 0)
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(15, 8)
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(23, 16)
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(31, 24)
          }.elsewhen(queue(id).address === io.bypass.address + 1.U) {
            laneStrobes(1) := true.B
            laneData(1) := queue(id).data(7, 0)
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(15, 8)
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(23, 16)
          }.elsewhen(queue(id).address === io.bypass.address + 2.U) {
            laneStrobes(2) := true.B
            laneData(2) := queue(id).data(7, 0)
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(15, 8)
          }.elsewhen(queue(id).address === io.bypass.address + 3.U) {
            laneStrobes(3) := true.B
            laneData(3) := queue(id).data(7, 0)
          }
        }
      }
    }
  }

  when(readPtr <= writePtr) {
    for (i <- 0 until depth) {
      when(i.U >= readPtr && i.U < writePtr) {
        bypassEntry(i)
      }
    }
  }.otherwise {
    for (i <- 0 until depth) {
      when(i.U >= readPtr) {
        bypassEntry(i)
      }
    }
    for (i <- 0 until depth) {
      when(i.U < writePtr) {
        bypassEntry(i)
      }
    }
  }

  io.bypass.data := laneData(3) ## laneData(2) ## laneData(1) ## laneData(0)
  io.bypass.strobe := laneStrobes(3) ## laneStrobes(2) ## laneStrobes(1) ## laneStrobes(0)

  // Empty logic
  io.flush.empty := !VecInit(queue.map(entry => entry.valid && entry.retire)).reduceTree(_ || _)

  // Recovery logic
  when(io.recover) {
    queue.foreach(item => {
      when(!item.retire) { // Skip item retired
        item.valid := false.B
      }
    })
  }

  // Retire logic
  io.retire.entries.foreach { entry =>
    when(entry.valid) {
      entry.id.foreach { id =>
        queue(id).retire := true.B
        queue(id).valid := true.B // Recovery bypass
      }
    }
  }
}

/** Store queue memory writer
  *
  * The output port of store queue
  */
class StoreQueueMemoryWriter extends Module {
  val io = IO(new Bundle {
    // Dequeue interface
    val deq = Flipped(DecoupledIO(new StoreQueueDequeueEntry()))
    // SMA interface
    val sma = new SMAWriterIO()
  })

  io.sma.valid := false.B
  io.sma.address := io.deq.bits.address
  io.sma.data := io.deq.bits.data
  io.sma.writeType := io.deq.bits.writeType

  io.deq.ready := false.B

  when(io.deq.valid) {
    when(io.deq.bits.valid) {
      io.sma.valid := true.B
      io.deq.ready := io.sma.ready
    }.otherwise { // Skip
      io.deq.ready := true.B
    }
  }
}
