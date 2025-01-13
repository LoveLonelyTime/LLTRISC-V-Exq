package bergamot.core.retire

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.record.ROBTableRetireIO
import bergamot.core.record.RegisterUpdateIO
import bergamot.core.record.CSRsWriteIO
import bergamot.core.record.StoreQueueRetireIO
import bergamot.core.record.ROBTableEntry
import bergamot.core.record.TrapRequestIO
import bergamot.core.record.MonitorIO
import bergamot.core.fetch.BranchPredictorUpdateIO
import bergamot.core.execute.LoadReservationUpdateIO
import bergamot.core.execute.LoadReservationEntry
import bergamot.core.execute.ExecuteResultEntry
import bergamot.core.debug.DebugIO

import bergamot.cache.FlushCacheIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Instruction retire
 *
 * When an instruction is in a non speculative and committed status, it will be retired,
 * and the retired instruction will actually change the core status.
 * It will detect the results of retired instructions, such as whether they cause prediction failures,
 * whether they trigger exceptions, and write memory.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Instruction retire
  *
  * @param depth
  *   ROB table depth
  */
class InstructionRetire(depth: Int) extends Module {
  require(depth > 0, "ROB table depth must be greater than 0")
  val io = IO(new Bundle {
    // Retired interface
    val retired = Flipped(DecoupledIO(DataType.receipt))
    // Table retire interface
    val robTableRetire = Flipped(new ROBTableRetireIO(depth))
    // Register update interface
    val registerUpdate = new RegisterUpdateIO()
    // Load reservation update interface
    val loadReservationUpdate = new LoadReservationUpdateIO()
    // Predictor update interface
    val predictorUpdate = new BranchPredictorUpdateIO()
    // Store queue interface
    val storeRetire = new StoreQueueRetireIO()
    // Recovery interface
    val recover = Output(new Bool())
    val correctPC = Output(DataType.address)
    // CSR write interface
    val csr = new CSRsWriteIO()
    // Trap interface
    val trap = new TrapRequestIO()
    // Monitor interface
    val monitor = new MonitorIO()
    // Flush interface
    val dCacheFlush = new FlushCacheIO()
    val l2DCacheFlush = new FlushCacheIO()
    val iCacheFlush = new FlushCacheIO()
    val tlbFlush = new FlushCacheIO()

    // Debug
    val debug = new DebugIO()
  })
  private val debugBreakpoint = RegInit(false.B)
  io.debug.hit := debugBreakpoint

  private object Status extends ChiselEnum {
    val retire, dCache, l2DCache, iCache, tlb = Value
  }

  private val statusReg = RegInit(Status.retire)

  private val flushID = RegInit(0.U)

  // Retire entries
  private val retireEntries = VecInit(
    io.robTableRetire.entries(io.retired.bits(30, 0) ## 0.U),
    io.robTableRetire.entries(io.retired.bits(30, 0) ## 1.U)
  )

  private val retireValid = retireEntries.map(item => item.commit || !item.valid)

  // Init interface
  io.recover := false.B
  io.correctPC := 0.U
  io.registerUpdate <> new RegisterUpdateIO().zero
  io.predictorUpdate <> new BranchPredictorUpdateIO().zero
  io.loadReservationUpdate <> new LoadReservationUpdateIO().zero
  io.storeRetire <> new StoreQueueRetireIO().zero
  io.csr <> new CSRsWriteIO().zero
  io.trap <> new TrapRequestIO().zero
  io.monitor <> new MonitorIO().zero
  io.dCacheFlush.req := false.B
  io.l2DCacheFlush.req := false.B
  io.iCacheFlush.req := false.B
  io.tlbFlush.req := false.B

  // Assertion functions
  private def hasException(entry: ROBTableEntry) =
    entry.valid && entry.executeResult.exception
  private def hasInterrupt(entry: ROBTableEntry) =
    entry.valid && io.trap.interruptPending
  private def hasBranch(entry: ROBTableEntry) =
    entry.valid && entry.executeResult.real =/= entry.spec
  private def hasCSR(entry: ROBTableEntry) =
    entry.valid && entry.executeResult.writeCSR
  private def hasXRet(entry: ROBTableEntry) =
    entry.valid && (entry.executeResult.mret || entry.executeResult.sret)
  private def hasFlush(entry: ROBTableEntry) =
    entry.valid && (entry.executeResult.flushDCache ||
      entry.executeResult.flushL2DCache ||
      entry.executeResult.invalidICache ||
      entry.executeResult.invalidTLB)

  // Path functions
  private def gotoExceptionHandler(entry: ROBTableEntry) = {
    io.recover := true.B
    io.trap.exceptionTrigger := true.B
    io.trap.trapPC := entry.pc
    io.trap.trapVal := entry.executeResult.exceptionVal
    io.trap.trapCode := entry.executeResult.exceptionCode
    io.correctPC := io.trap.handlerPC

    io.retired.ready := true.B

    // when(debugBreakpoint) {
    //   printf("Exception!!!! pc = %x, cause = %d,to = %x\n", entry.pc, entry.executeResult.exceptionCode, io.trap.handlerPC)
    // }
  }

  private def gotoInterruptHandler(entry: ROBTableEntry) = {
    io.recover := true.B
    io.trap.interruptTrigger := true.B
    io.trap.trapPC := entry.pc
    io.trap.trapVal := 0.U // TODO
    io.correctPC := io.trap.handlerPC

    io.retired.ready := true.B
    // when(debugBreakpoint) {
    //   printf("Interrupt!!!! pc = %x to= %x\n", entry.pc, io.trap.handlerPC)
    // }
  }

  private def gotoRecoveryPath(entry: ROBTableEntry) = {
    io.recover := true.B
    io.correctPC := entry.executeResult.real

    io.retired.ready := true.B

    // when(debugBreakpoint) {
    // printf(
    //   "spec violate!!!: pc = %x, sepc = %x, real = %x\n",
    //   entry.pc,
    //   entry.spec,
    //   entry.executeResult.real
    // )
    // }
  }

  private def gotoXRetPath(entry: ROBTableEntry) = {
    when(entry.executeResult.mret) {
      io.trap.mret := true.B
    }.elsewhen(entry.executeResult.sret) {
      io.trap.sret := true.B
    }

    io.recover := true.B
    io.correctPC := io.trap.handlerPC

    io.retired.ready := true.B

    // when(debugBreakpoint) {
    //   printf(
    //     "xret !!!: pc = %x\n",
    //     entry.pc
    //   )
    // }
  }

  private def gotoCSRPath(entry: ROBTableEntry) = {
    io.recover := true.B
    io.correctPC := entry.executeResult.real

    io.retired.ready := true.B
  }

  private def gotoFlushPath(id: Int) = {
    statusReg := Status.dCache
    flushID := id.U
  }

  // Update functions
  private def updateRegister(id: Int) = {
    io.registerUpdate.entries(id).rd := retireEntries(id).rd
    io.registerUpdate.entries(id).result := retireEntries(id).executeResult.result

    // Update predictor
    when(retireEntries(id).executeResult.branch) {
      io.predictorUpdate.entries(id).valid := true.B
      io.predictorUpdate.entries(id).jump := retireEntries(id).executeResult.real =/= retireEntries(id).executeResult.next
      io.predictorUpdate.entries(id).pc := retireEntries(id).pc
      io.predictorUpdate.entries(id).address := retireEntries(id).executeResult.real
    }

    // when(retireEntries(id).pc === "hc00146b4".U) {
    //   debugBreakpoint := true.B
    // }
    // when(debugBreakpoint) {
    // printf(
    //   "retired instruction: pc = %x , r = %x, v = %d\n",
    //   retireEntries(id).pc,
    //   retireEntries(id).executeResult.result,
    //   retireEntries(id).valid
    // )
    // }
  }

  private def retireStoreQueue(id: Int) = {
    when(retireEntries(id).executeResult.write) {
      io.storeRetire.entries(id).valid := true.B
      io.storeRetire.entries(id).id := retireEntries(id).executeResult.writeID
    }
  }

  private def writeCSRs(id: Int) = {
    io.csr.wen := true.B
    io.csr.address := retireEntries(id).executeResult.csrAddress
    io.csr.data := retireEntries(id).executeResult.csrData
  }

  private def updateLoadReservation(id: Int) = {
    when(retireEntries(id).executeResult.sc) {
      io.loadReservationUpdate.load := false.B
      io.loadReservationUpdate.valid := true.B
    }.elsewhen(retireEntries(id).executeResult.lr) {
      io.loadReservationUpdate.load := true.B
      io.loadReservationUpdate.address := retireEntries(id).executeResult.lrAddress
      io.loadReservationUpdate.valid := true.B
    }
  }

  io.retired.ready := false.B
  when(statusReg === Status.retire && io.retired.valid && retireValid.reduce(_ && _)) {
    val grant = VecInit2(false.B)
    val retired = VecInit2(false.B)
    io.monitor.instret := retired.foldLeft(0.U)(_ + _)

    retireEntries.zipWithIndex.foreach { case (entry, id) =>
      val granted = if (id == 0) true.B else grant(id - 1)
      when(granted) {
        when(hasException(entry)) { // Exception ?
          gotoExceptionHandler(entry)
          retired(id) := true.B
        }.elsewhen(hasInterrupt(entry)) {
          gotoInterruptHandler(entry)
        }.elsewhen(hasXRet(entry)) { // XRet ?
          gotoXRetPath(entry)
          retired(id) := true.B
        }.elsewhen(hasCSR(entry)) { // CSR ?
          writeCSRs(id)
          updateRegister(id)
          gotoCSRPath(entry)
          retired(id) := true.B
        }.elsewhen(hasBranch(entry)) { // Branch ?
          updateRegister(id)
          gotoRecoveryPath(entry)
          retired(id) := true.B
        }.elsewhen(hasFlush(entry)) { // Flush ?
          gotoFlushPath(id)
          retired(id) := true.B
        }.otherwise {
          when(entry.valid) {
            updateRegister(id)
            updateLoadReservation(id)
            retireStoreQueue(id)
            retired(id) := true.B
          }
          grant(id) := true.B

          if (id == retireEntries.length - 1) {
            io.retired.ready := true.B
          }
        }
      }
    }
  }

  // Flush process
  // dCache -> l2DCache -> iCache -> TLB
  when(statusReg === Status.dCache) {
    when(retireEntries(flushID).executeResult.flushDCache) {
      io.dCacheFlush.req := true.B

      when(io.dCacheFlush.empty) { // Finish
        statusReg := Status.l2DCache
      }
    }.otherwise {
      statusReg := Status.l2DCache
    }
  }

  when(statusReg === Status.l2DCache) {
    when(retireEntries(flushID).executeResult.flushL2DCache) {
      io.l2DCacheFlush.req := true.B

      when(io.l2DCacheFlush.empty) { // Finish
        statusReg := Status.iCache
      }
    }.otherwise {
      statusReg := Status.iCache
    }
  }

  when(statusReg === Status.iCache) {
    when(retireEntries(flushID).executeResult.invalidICache) {
      io.iCacheFlush.req := true.B

      when(io.iCacheFlush.empty) { // Finish
        statusReg := Status.tlb
      }
    }.otherwise {
      statusReg := Status.tlb
    }
  }

  when(statusReg === Status.tlb) {
    when(retireEntries(flushID).executeResult.invalidTLB) {
      io.tlbFlush.req := true.B

      when(io.tlbFlush.empty) { // Finish
        statusReg := Status.retire
        // when(debugBreakpoint) {
        //   printf("Fench pc = %d\n", retireEntries(flushID).pc)
        // }
        io.recover := true.B
        io.correctPC := retireEntries(flushID).executeResult.real
        io.retired.ready := true.B
      }
    }.otherwise {
      statusReg := Status.retire
      // when(debugBreakpoint) {
      //   printf("Fench pc = %d\n", retireEntries(flushID).pc)
      // }
      io.recover := true.B
      io.correctPC := retireEntries(flushID).executeResult.real
      io.retired.ready := true.B
    }
  }
}
