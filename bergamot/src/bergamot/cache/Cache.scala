package bergamot.cache

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.MemoryAccessLength

import bergamot.bus.SMAReaderIO
import bergamot.bus.SMAWriterIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._
import bergamot.utils.ReadWriteSRAM

/*
 * RAM Cache
 *
 * Default cache implementations:
 * - SetCache: Set associative cache (Write-Through without allocation)
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Set associative cache (Write-Through)
  *
  * This implementation uses a set associative strategy and write-through(without allocation).
  *
  * Using random replacement algorithm
  *
  * @param wayDepth
  *   Way depth
  * @param cacheLineDepth
  *   Cache line depth
  * @param cacheCellDepth
  *   Require 32-bit aligned cache cell depth
  */
class SetCache(wayDepth: Int, cacheLineDepth: Int, cacheCellDepth: Int) extends Module {
  require(wayDepth > 0, "Insufficient ways")
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Insufficient lines")
  require(cacheCellDepth > 0 && cacheCellDepth % 2 == 0, "Require 32-bit aligned cache cell depth")

  val io = IO(new Bundle {
    // In
    val inWriter = Flipped(new SMAWriterIO())
    val inReader = Flipped(new CacheLineRequestIO(cacheCellDepth))

    // Out
    val outWriter = new SMAWriterIO()
    val outReader = new CacheLineRequestIO(cacheCellDepth)

    // Flush: invalidate all entries
    val flush = Flipped(new FlushCacheIO())
  })

  private val split = splitCacheLineAddress(cacheLineDepth, cacheCellDepth) _

  // Cache memory
  // tagMem: [31,1] Tag, [0] valid
  private val tagMem = Seq.fill(wayDepth)(Module(new ReadWriteSRAM(DataType.address)(cacheLineDepth)))
  private val dataMem = Seq.fill(wayDepth)(Module(new ReadWriteSRAM(Vec(cacheCellDepth, UInt(CoreConstant.cacheCellLength.W)))(cacheLineDepth)))

  private val victimIncr = WireInit(false.B)
  private val (victimReg, _) = pointer(wayDepth, victimIncr)

  private object Status extends ChiselEnum {
    val idle, lookup, read, write = Value;
  }

  private val flushWorkingReg = RegInit(true.B) // Flush on reset
  private val flushIncr = WireInit(false.B)
  private val (flushReg, _) = pointer(cacheLineDepth, flushIncr)

  private val statusReg = RegInit(Status.idle)

  // Arbitration for requesting
  private val responseReadReg = RegInit(false.B)

  private def flushGuard(f: => Unit) =
    when(!flushWorkingReg) { f }

  // Init
  io.inWriter <> new SMAWriterIO().zero
  io.inReader <> new CacheLineRequestIO(cacheCellDepth).zero
  io.outWriter <> new SMAWriterIO().zero
  io.outReader <> new CacheLineRequestIO(cacheCellDepth).zero
  io.flush <> new FlushCacheIO().zero

  tagMem.foreach { item =>
    item.io.dataIn := 0.U
    item.io.addr := 0.U
    item.io.write := false.B
  }
  dataMem.foreach { item =>
    item.io.dataIn := Vec(cacheCellDepth, UInt(CoreConstant.cacheCellLength.W)).zero
    item.io.addr := 0.U
    item.io.write := false.B
  }

  when(flushWorkingReg) { // Flushing
    tagMem.foreach { tagWay =>
      tagWay.io.addr := flushReg
      tagWay.io.dataIn := 0.U
      tagWay.io.write := true.B
    }

    when(flushReg === (cacheLineDepth - 1).U) { // Finish
      flushWorkingReg := false.B
      io.flush.empty := true.B // Response
    }.otherwise {
      flushIncr := true.B
    }
  }

  switch(statusReg) {
    is(Status.idle) {
      // Priority: Flush >| Read > Write
      when(io.flush.req && !flushWorkingReg) {
        flushWorkingReg := true.B
        flushReg := 0.U
      }

      val address = split(Mux(io.inReader.valid, io.inReader.address, io.inWriter.address))

      flushGuard { // Address port guard
        tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
          tagWay.io.addr := address.index
          dataWay.io.addr := address.index
        }
      }

      when(io.inReader.valid) {
        responseReadReg := true.B
        statusReg := Status.lookup
      }.elsewhen(io.inWriter.valid) {
        responseReadReg := false.B
        statusReg := Status.lookup
      }
    }

    is(Status.lookup) {
      val address = split(Mux(responseReadReg, io.inReader.address, io.inWriter.address))
      val hit = WireInit(false.B)

      flushGuard { // Address port guard
        tagMem.zip(dataMem).foreach { case (tagWay, dataWay) =>
          tagWay.io.addr := address.index
          dataWay.io.addr := address.index

          val tagCell = tagWay.io.dataOut
          val dataCell = dataWay.io.dataOut

          when(tagCell(0) && tagCell(31, 1) === address.tag) { // Hit
            hit := true.B
            when(responseReadReg) { // Hit read
              io.inReader.error := false.B
              io.inReader.data := dataCell
              io.inReader.ready := true.B

              statusReg := Status.idle
            }.otherwise { // Hit write
              val newData = WireInit(dataCell)
              switch(io.inWriter.writeType) {
                is(MemoryAccessLength.byte) {
                  newData(address.offset) := Mux(address.byte.asBool, io.inWriter.data(7, 0) ## dataCell(address.offset)(7, 0), dataCell(address.offset)(15, 8) ## io.inWriter.data(7, 0))
                }
                is(MemoryAccessLength.half) {
                  newData(address.offset) := io.inWriter.data(15, 0)
                }
                is(MemoryAccessLength.word) {
                  newData(address.offset) := io.inWriter.data(15, 0)
                  newData(address.offset + 1.U) := io.inWriter.data(31, 16)
                }
              }
              dataWay.io.dataIn := newData
              dataWay.io.write := true.B
            }
          }
        }
      }

      when(responseReadReg && !hit) { // Miss
        statusReg := Status.read
      }.elsewhen(!responseReadReg) { // Write-Through
        statusReg := Status.write
      }
    }

    is(Status.read) {
      val address = split(io.inReader.address)

      io.outReader.address := io.inReader.address
      io.outReader.valid := true.B

      when(io.outReader.ready) {

        flushGuard { // Address port guard
          tagMem.zip(dataMem).zipWithIndex.foreach { case ((tagWay, dataWay), way) =>
            when(way.U === victimReg) {
              tagWay.io.addr := address.index
              tagWay.io.dataIn := address.tag ## 1.U
              tagWay.io.write := !io.outReader.error // Do not write when an error occurs

              dataWay.io.addr := address.index
              dataWay.io.dataIn := io.outReader.data
              dataWay.io.write := !io.outReader.error
            }
          }
        }

        victimIncr := true.B
        io.inReader.error := io.outReader.error
        io.inReader.data := io.outReader.data
        io.inReader.ready := true.B
        statusReg := Status.idle
      }
    }

    is(Status.write) {
      io.inWriter <> io.outWriter
      when(io.outWriter.ready) {
        statusReg := Status.idle
      }
    }
  }
}

/** Cache line request to SMA
  *
  * Conversion of cache line requests to SMA interface with 32-bit bandwidth.
  *
  * @param cacheLineDepth
  *   Cache line depth
  * @param cacheCellDepth
  *   Require 32-bit aligned cache cell depth
  */
class CacheLineRequest2SMA(cacheLineDepth: Int, cacheCellDepth: Int) extends Module {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Insufficient lines")
  require(cacheCellDepth > 0 && cacheCellDepth % 2 == 0, "Require 32-bit aligned cache cell depth")

  val io = IO(new Bundle {
    val request = Flipped(new CacheLineRequestIO(cacheCellDepth))
    val smaReader = new SMAReaderIO()
  })

  private object Status extends ChiselEnum {
    val idle, working, finish = Value
  }

  private val statusReg = RegInit(Status.idle)
  private val errorReg = RegInit(false.B)

  // counterReg: Current cell pointer
  private val incr = WireInit(false.B)
  private val (counterReg, nextValue) = pointer(cacheCellDepth / 2, incr)

  // Cache line buffer
  private val dataReg = RegInit(Vec(cacheCellDepth, UInt(CoreConstant.cacheCellLength.W)).zero)

  when(statusReg === Status.idle && io.request.valid) {
    statusReg := Status.working
    errorReg := false.B
  }

  private val split = splitCacheLineAddress(cacheLineDepth, cacheCellDepth) _
  private val address = split(io.request.address)

  io.smaReader.valid := false.B
  io.smaReader.readType := MemoryAccessLength.word
  io.smaReader.address := address.tag ## address.index ## counterReg ## 0.U(2.W)

  when(statusReg === Status.working) {
    io.smaReader.valid := true.B

    when(io.smaReader.ready) {
      incr := true.B
      dataReg(counterReg ## 1.U) := io.smaReader.data(31, 16)
      dataReg(counterReg ## 0.U) := io.smaReader.data(15, 0)
      errorReg := errorReg || io.smaReader.error

      when(counterReg === (cacheCellDepth / 2 - 1).U) { // Finish
        statusReg := Status.finish
      }
    }
  }

  io.request.ready := false.B
  io.request.error := errorReg
  io.request.data := dataReg

  when(statusReg === Status.finish) {
    io.request.ready := true.B
    statusReg := Status.idle
  }
}

/** SMA to cache line request
  *
  * Conversion of SMA interface to cache line requests
  *
  * @param cacheLineDepth
  *   Cache line depth
  * @param cacheCellDepth
  *   Require 32-bit aligned cache cell depth
  */
class SMA2CacheLineRequest(cacheLineDepth: Int, cacheCellDepth: Int) extends Module {
  require(cacheLineDepth > 0 && cacheLineDepth % 2 == 0, "Insufficient lines")
  require(cacheCellDepth > 0 && cacheCellDepth % 2 == 0, "Require 32-bit aligned cache cell depth")

  val io = IO(new Bundle {
    val request = new CacheLineRequestIO(cacheCellDepth)
    val smaReader = Flipped(new SMAReaderIO())
  })

  io.request.valid := io.smaReader.valid
  io.request.address := io.smaReader.address

  io.smaReader.ready := io.request.ready
  io.smaReader.error := io.request.error
  io.smaReader.data := 0.U

  private val split = splitCacheLineAddress(cacheLineDepth, cacheCellDepth) _
  private val address = split(io.smaReader.address)

  switch(io.smaReader.readType) {
    is(MemoryAccessLength.byte) {
      io.smaReader.data := Mux(
        address.byte.asBool,
        io.request.data(address.offset)(15, 8),
        io.request.data(address.offset)(7, 0)
      )
    }
    is(MemoryAccessLength.half) {
      io.smaReader.data := io.request.data(address.offset)
    }
    is(MemoryAccessLength.word) {
      io.smaReader.data := io.request.data(address.offset + 1.U) ## io.request.data(address.offset)
    }
  }
}
