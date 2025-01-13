package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.decode.InstructionType
import bergamot.core.record.TLBRequestIO
import bergamot.core.record.StoreQueueAllocIO
import bergamot.core.record.ExceptionCode

import bergamot.bus.SMAReaderIO
import bergamot.bus.SMAWriterIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Memory operation unit, which is suitable for memory operations
 *
 * List of supported instructions:
 * - I: sb, sh, sw, lb, lh, lw, lbu, lhu
 * - A: lr.w, sc.w, amoswap.w, amoadd.w, amoxor.w, amoand.w, amoor.w, amomin.w, amomax.w, amominu.w, amomaxu.w
 * - FD: flw, fsw, fld, fsd
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Memory components
  *
  * MemoryDecodeStage -> MemoryExecuteStage -> MemoryTLBStage -> MemoryReadWriteStage
  */
class Memory extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // DTLB interface
    val dtlb = new TLBRequestIO()
    // SMA interface
    val sma = new SMAReaderIO()
    // Store queue interface
    val alloc = new StoreQueueAllocIO()
    val loadReservationUpdate = Flipped(new LoadReservationUpdateIO())
    // Recovery logic
    val recover = Input(Bool())
  })

  private val memoryDecodeStage = Module(new MemoryDecodeStage())
  private val memoryExecuteStage = Module(new MemoryExecuteStage())
  private val memoryTLBStage = Module(new MemoryTLBStage())
  private val memoryReadWriteStage = Module(new MemoryReadWriteStage())

  io.in <> memoryDecodeStage.io.in
  memoryDecodeStage.io.out <> memoryExecuteStage.io.in
  memoryExecuteStage.io.out <> memoryTLBStage.io.in
  memoryTLBStage.io.out <> memoryReadWriteStage.io.in
  memoryReadWriteStage.io.out <> io.out

  memoryTLBStage.io.dtlb <> io.dtlb
  memoryReadWriteStage.io.sma <> io.sma
  memoryReadWriteStage.io.alloc <> io.alloc
  memoryReadWriteStage.io.loadReservationUpdate <> io.loadReservationUpdate

  memoryDecodeStage.io.recover := io.recover
  memoryExecuteStage.io.recover := io.recover
  memoryTLBStage.io.recover := io.recover
  memoryReadWriteStage.io.recover := io.recover
}

/** Memory decode stage
  *
  * Identify memory access types and address
  *
  * Single cycle stage
  */
class MemoryDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new MemoryExecuteStageEntry())
    // Recovery logic
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new ExecuteEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // Decode logic
  // op
  io.out.bits.op := MemoryOperationType.undefined
  switch(inReg.instructionType) {
    is(InstructionType.I) {
      io.out.bits.op := MuxLookup(inReg.func3, MemoryOperationType.undefined)(
        Seq(
          "b000".U -> MemoryOperationType.lb,
          "b001".U -> MemoryOperationType.lh,
          "b010".U -> MemoryOperationType.lw,
          "b011".U -> MemoryOperationType.ld,
          "b100".U -> MemoryOperationType.lbu,
          "b101".U -> MemoryOperationType.lhu
        )
      )
    }
    is(InstructionType.S) {
      io.out.bits.op := MuxLookup(inReg.func3, MemoryOperationType.undefined)(
        Seq(
          "b000".U -> MemoryOperationType.sb,
          "b001".U -> MemoryOperationType.sh,
          "b010".U -> MemoryOperationType.sw,
          "b011".U -> MemoryOperationType.sd
        )
      )
    }
    is(InstructionType.R) {
      io.out.bits.op := MuxLookup(inReg.func7(6, 2), MemoryOperationType.undefined)(
        Seq(
          "b00010".U -> MemoryOperationType.lr,
          "b00011".U -> MemoryOperationType.sc,
          "b00001".U -> MemoryOperationType.amoswap,
          "b00000".U -> MemoryOperationType.amoadd,
          "b00100".U -> MemoryOperationType.amoxor,
          "b01100".U -> MemoryOperationType.amoand,
          "b01000".U -> MemoryOperationType.amoor,
          "b10000".U -> MemoryOperationType.amomin,
          "b10100".U -> MemoryOperationType.amomax,
          "b11000".U -> MemoryOperationType.amominu,
          "b11100".U -> MemoryOperationType.amomaxu
        )
      )
    }
  }

  // add1 & add2
  io.out.bits.add1 := inReg.rs1.receipt
  // For LR/SC and AMO, there is no immediate address
  io.out.bits.add2 := Mux(inReg.instructionType === InstructionType.R, 0.U, signExtended(inReg.imm, 11))

  // op1: the data stored
  io.out.bits.op1 := inReg.rs2.receipt

  io.out.bits.rd := inReg.rd.receipt
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

/** Memory execute stage
  *
  * Calculate memory access virtual address
  *
  * Single cycle stage
  */
class MemoryExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new MemoryExecuteStageEntry()))
    val out = DecoupledIO(new MemoryTLBStageEntry())
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new MemoryExecuteStageEntry().zero)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.in.ready := io.out.ready

  // Execute logic
  private val vaddress = WireInit(inReg.add1 + inReg.add2) // Address addition
  io.out.bits.op := inReg.op
  io.out.bits.vaddress := vaddress
  io.out.bits.op1 := inReg.op1

  // Misaligned address check
  io.out.bits.error := MemoryErrorCode.none
  when(
    ((inReg.op in (MemoryOperationType.wordValues ++ MemoryOperationType.doubleValues)) && vaddress(1, 0) =/= 0.U) || // Word/double 4bytes
      ((inReg.op in MemoryOperationType.halfValues) && vaddress(0) =/= 0.U) // Half 2bytes
  ) {
    io.out.bits.error := MemoryErrorCode.misaligned
  }

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

/** Memory TLB stage
  *
  * Accessing TLB to calculate physical addresses
  *
  * Waiting for TLB
  */
class MemoryTLBStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new MemoryTLBStageEntry()))
    val out = DecoupledIO(new MemoryReadWriteStageEntry())
    // DTLB interface
    val dtlb = new TLBRequestIO()
    // Recovery interface
    val recover = Input(Bool())
  })
  private val statusReg = RegInit(Status.idle)
  private object Status extends ChiselEnum {
    val idle, request = Value
  }

  // Pipeline logic
  private val inReg = RegInit(new MemoryTLBStageEntry().zero)
  private val error = RegInit(MemoryErrorCode().zero)
  private val paddress = RegInit(DataType.address.zeroAsUInt)

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits

    when(io.in.bits.valid && !io.recover && io.in.bits.error === MemoryErrorCode.none) { // Effective access
      statusReg := Status.request
    }
  }

  io.in.ready := statusReg === Status.idle && io.out.ready // Idle

  // TLB
  io.dtlb <> new TLBRequestIO().zero
  when(statusReg === Status.request) {
    io.dtlb.valid := true.B
    io.dtlb.vaddress := inReg.vaddress
    io.dtlb.write := (inReg.op in MemoryOperationType.writeValues)

    when(io.dtlb.ready) { // OK
      error := io.dtlb.error
      paddress := io.dtlb.paddress
      statusReg := Status.idle
    }
  }

  io.out.bits.op := inReg.op
  io.out.bits.error := Mux(inReg.error =/= MemoryErrorCode.none, inReg.error, error) // Priority

  io.out.bits.vaddress := inReg.vaddress
  io.out.bits.paddress := paddress
  io.out.bits.op1 := inReg.op1

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := statusReg === Status.idle

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
    // To ensure TLB integrity, do not undo FSM
  }
}

/** Memory read write stage
  *
  * Access storage through SMA interface and commit to store queue
  *
  * Waiting for SMA interface and store queue
  */
class MemoryReadWriteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new MemoryReadWriteStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // SMA interface
    val sma = new SMAReaderIO()
    // Store queue interface
    val alloc = new StoreQueueAllocIO()
    // Load reservation update interface
    val loadReservationUpdate = Flipped(new LoadReservationUpdateIO())
    // Recovery interface
    val recover = Input(Bool())
  })

  private val loadReservation = RegInit(new LoadReservationEntry().zero)
  private val recoveryLoadReservation = RegInit(new LoadReservationEntry().zero)

  private val statusReg = RegInit(Status.idle)

  private object Status extends ChiselEnum {
    val idle, readTop, readBottom, writeTop, writeBottom = Value
  }

  // Pipeline logic
  private val inReg = RegInit(new MemoryReadWriteStageEntry().zero)
  private val readResult = RegInit(DataType.operation.zeroAsUInt)
  private val readError = RegInit(false.B)
  private val allocID = RegInit(VecInit2(DataType.receipt.zero))
  private def scSuccess(vaddress: UInt) = vaddress === loadReservation.address && loadReservation.valid

  io.in.ready := statusReg === Status.idle && io.out.ready // Idle

  when(io.out.fire) { // Stall
    inReg.valid := false.B

    // SC
    when(inReg.valid && inReg.op === MemoryOperationType.sc) {
      // Clear load reservation
      loadReservation.valid := false.B
    }
  }

  when(io.in.fire) { // Sample
    inReg := io.in.bits

    when(io.in.bits.valid && !io.recover && io.in.bits.error === MemoryErrorCode.none) { // Effective access
      when(io.in.bits.op in MemoryOperationType.readValues) {
        readResult := 0.U
        readError := false.B

        statusReg := Status.readBottom
      }.elsewhen(io.in.bits.op in MemoryOperationType.writeValues) {
        when(io.in.bits.op =/= MemoryOperationType.sc || scSuccess(io.in.bits.vaddress)) {
          statusReg := Status.writeBottom
        }
      }
    }
  }

  // Read FSM
  io.sma <> new SMAReaderIO().zero
  when(statusReg in (Status.readBottom, Status.readTop)) {
    io.sma.valid := true.B
    // Address mux
    io.sma.address := Mux(statusReg === Status.readBottom, inReg.paddress, inReg.paddress + 4.U)

    when(inReg.op in MemoryOperationType.byteValues) {
      io.sma.readType := MemoryAccessLength.byte
    }.elsewhen(inReg.op in MemoryOperationType.halfValues) {
      io.sma.readType := MemoryAccessLength.half
    }.elsewhen(inReg.op in MemoryOperationType.wordValues) {
      io.sma.readType := MemoryAccessLength.word
    }

    when(io.sma.ready) {
      // Concat
      readResult := Mux(
        statusReg === Status.readBottom,
        extractBits(CoreConstant.wordWidth)(readResult, 1) ## io.sma.data,
        io.sma.data ## extractBits(CoreConstant.wordWidth)(readResult, 0)
      )

      readError := readError || io.sma.error
      statusReg := Status.idle

      // Do it need to go to the next step?
      when(!io.sma.error && inReg.valid && !io.recover) {
        when(inReg.op === MemoryOperationType.lr) { // lr
          loadReservation.address := inReg.vaddress
          loadReservation.valid := true.B
        }.elsewhen(inReg.op in MemoryOperationType.amoValues) { // AMO
          statusReg := Status.writeBottom
        }.elsewhen(inReg.op === MemoryOperationType.ld && statusReg === Status.readBottom) { // ld
          statusReg := Status.readTop
        }
      }
    }
  }

  // Write FSM
  io.alloc <> new StoreQueueAllocIO().zero
  when(statusReg in (Status.writeBottom, Status.writeTop)) {
    io.alloc.valid := true.B
    // AMO
    val writeData = WireInit(inReg.op1)
    val gtu = WireInit(inReg.op1 > readResult)
    val gt = WireInit(false.B)
    val sign = inReg.op1(31) ## readResult(31)
    switch(sign) {
      is("b00".U) { gt := gtu }
      is("b01".U) { gt := true.B }
      is("b10".U) { gt := false.B }
      is("b11".U) { gt := gtu }
    }
    switch(inReg.op) {
      is(MemoryOperationType.amoadd) { writeData := readResult + inReg.op1 }
      is(MemoryOperationType.amoxor) { writeData := readResult ^ inReg.op1 }
      is(MemoryOperationType.amoand) { writeData := readResult & inReg.op1 }
      is(MemoryOperationType.amoor) { writeData := readResult | inReg.op1 }
      is(MemoryOperationType.amomax) { writeData := Mux(gt, inReg.op1, readResult) }
      is(MemoryOperationType.amomaxu) { writeData := Mux(gtu, inReg.op1, readResult) }
      is(MemoryOperationType.amomin) { writeData := Mux(gt, readResult, inReg.op1) }
      is(MemoryOperationType.amominu) { writeData := Mux(gtu, readResult, inReg.op1) }
    }

    // Address & data mux
    when(statusReg === Status.writeBottom) {
      io.alloc.data := extractBits(CoreConstant.wordWidth)(writeData, 0)
      io.alloc.address := inReg.paddress
    }.otherwise {
      io.alloc.data := extractBits(CoreConstant.wordWidth)(writeData, 1)
      io.alloc.address := inReg.paddress + 4.U
    }

    when(inReg.op in MemoryOperationType.byteValues) {
      io.alloc.writeType := MemoryAccessLength.byte
    }.elsewhen(inReg.op in MemoryOperationType.halfValues) {
      io.alloc.writeType := MemoryAccessLength.half
    }.elsewhen(inReg.op in MemoryOperationType.wordValues) {
      io.alloc.writeType := MemoryAccessLength.word
    }

    when(io.alloc.ready) {
      when(statusReg === Status.writeBottom) {
        allocID(0) := io.alloc.id
      }.otherwise {
        allocID(1) := io.alloc.id
      }

      // Do it need to go to the next step?
      statusReg := Status.idle
      when(inReg.op === MemoryOperationType.sd && statusReg === Status.writeBottom) {
        statusReg := Status.writeTop
      }
    }
  }

  // Result
  io.out.bits := new ExecuteResultEntry().zero

  when(inReg.op === MemoryOperationType.lr) {
    io.out.bits.resultLR(inReg.vaddress)
  }

  when(inReg.op === MemoryOperationType.lb) {
    io.out.bits.result := signExtended(readResult, 7)
  }.elsewhen(inReg.op === MemoryOperationType.lbu) {
    io.out.bits.result := readResult(7, 0)
  }.elsewhen(inReg.op === MemoryOperationType.lh) {
    io.out.bits.result := signExtended(readResult, 15)
  }.elsewhen(inReg.op === MemoryOperationType.lhu) {
    io.out.bits.result := readResult(15, 0)
  }.elsewhen(inReg.op in (MemoryOperationType.lw :: MemoryOperationType.ld :: MemoryOperationType.lr :: MemoryOperationType.amoValues)) {
    io.out.bits.result := readResult
  }.elsewhen(inReg.op === MemoryOperationType.sc) {
    io.out.bits.result := Mux(scSuccess(inReg.vaddress), 0.U, 1.U)
    io.out.bits.sc := true.B
  }

  // Alloc ID
  when(inReg.op === MemoryOperationType.sc) { // sc
    when(scSuccess(inReg.vaddress)) {
      io.out.bits.resultMemory(VecInit(allocID(0), allocID(0)))
    }
  }.elsewhen(inReg.op === MemoryOperationType.sd) { // sd
    io.out.bits.resultMemory(VecInit(allocID(0), allocID(1)))
  }.elsewhen(inReg.op in MemoryOperationType.writeValues) {
    io.out.bits.resultMemory(VecInit(allocID(0), allocID(0)))
  }

  // Exception
  when((inReg.op in MemoryOperationType.readValues) && readError) {
    io.out.bits.triggerException(ExceptionCode.loadAccessFault.U, inReg.vaddress)
  }

  // Last error
  switch(inReg.error) {
    is(MemoryErrorCode.misaligned) {
      when(inReg.op in MemoryOperationType.writeValues) {
        io.out.bits.triggerException(ExceptionCode.storeAMOAddressMisaligned.U, inReg.vaddress)
      }.elsewhen(inReg.op in MemoryOperationType.readValues) {
        io.out.bits.triggerException(ExceptionCode.loadAddressMisaligned.U, inReg.vaddress)
      }
    }

    is(MemoryErrorCode.pageFault) {
      when(inReg.op in MemoryOperationType.writeValues) {
        io.out.bits.triggerException(ExceptionCode.storeAMOPageFault.U, inReg.vaddress)
      }.elsewhen(inReg.op in MemoryOperationType.readValues) {
        io.out.bits.triggerException(ExceptionCode.loadPageFault.U, inReg.vaddress)
      }
    }

    is(MemoryErrorCode.memoryFault) {
      when(inReg.op in MemoryOperationType.writeValues) {
        io.out.bits.triggerException(ExceptionCode.storeAMOAccessFault.U, inReg.vaddress)
      }.elsewhen(inReg.op in MemoryOperationType.readValues) {
        io.out.bits.triggerException(ExceptionCode.loadAccessFault.U, inReg.vaddress)
      }
    }
  }

  when(inReg.op === MemoryOperationType.undefined) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
  }

  // rd & pc & valid
  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.real := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := statusReg === Status.idle

  // Update load reservation
  when(io.loadReservationUpdate.valid) {
    when(io.loadReservationUpdate.load) { // LR
      recoveryLoadReservation.address := io.loadReservationUpdate.address
      recoveryLoadReservation.valid := true.B
    }.otherwise { // SC
      recoveryLoadReservation.valid := false.B
    }
  }

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
    // Undo write FSM to prevent writing to store queue
    when(statusReg in (Status.writeBottom, Status.writeTop)) {
      statusReg := Status.idle
    }

    loadReservation := recoveryLoadReservation
  }
}
