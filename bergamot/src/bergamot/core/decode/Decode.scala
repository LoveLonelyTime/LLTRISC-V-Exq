package bergamot.core.decode

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.broadcast.DataBroadcastIO
import bergamot.core.record.RegisterMappingIO
import bergamot.core.record.ROBTableWriteIO
import bergamot.core.execute.MemoryErrorCode
import bergamot.core.execute.ExecuteQueueType
import bergamot.core.execute.ExecuteQueueEnqueueIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Instruction decode
 *
 * The decoder decodes instructions and dispatches them according to the type of execution queue.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Decode components
  *
  * The instruction decoding stage is divided into three cycles: DecodeStage -> RegisterMappingStage -> IssueStage.
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class Decode(executeQueueWidth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(Vec2(new DecodeStageEntry())))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // Broadcast interface
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interface
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
    // ROB table write interface
    val robTableWrite = new ROBTableWriteIO()
    // Recovery interface
    val recover = Input(Bool())
  })

  // Pipeline stages
  private val decodeStage = Module(new DecodeStage())
  private val registerMappingStage = Module(new RegisterMappingStage())
  private val issueStage = Module(new IssueStage(executeQueueWidth))

  io.in <> decodeStage.io.in
  decodeStage.io.out <> registerMappingStage.io.in
  registerMappingStage.io.out <> issueStage.io.in
  issueStage.io.enqs <> io.enqs

  registerMappingStage.io.mapping <> io.mapping
  registerMappingStage.io.robTableWrite <> io.robTableWrite

  issueStage.io.broadcast <> io.broadcast

  decodeStage.io.recover := io.recover
  registerMappingStage.io.recover := io.recover
  issueStage.io.recover := io.recover
}

/** Decode stage
  *
  * Decode according to instruction format
  *
  * Single cycle stage
  */
class DecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec2(new DecodeStageEntry())))
    val out = DecoupledIO(Vec2(new RegisterMappingStageEntry()))
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(Vec2(new DecodeStageEntry()).zero)

  io.in.ready := io.out.ready
  when(io.out.fire) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Decode logic
  inReg.zip(io.out.bits).foreach { case (in, out) =>
    // opcode
    out.opcode := in.instruction(6, 0)

    // Instruction Type
    out.instructionType := WireInit(InstructionType.UNK)
    switch(in.instruction(6, 2)) {
      // I: lui
      is("b01101".U) {
        out.instructionType := InstructionType.U
      }
      // I: auipc
      is("b00101".U) {
        out.instructionType := InstructionType.U
      }
      // I: beq, bne, blt, bge, bltu, bgeu
      is("b11000".U) {
        out.instructionType := InstructionType.B
      }
      // I: sb, sh, sw
      is("b01000".U) {
        out.instructionType := InstructionType.S
      }
      // I: add, sub, sll, slt, sltu, xor, srl, sra, or, and
      // M: mul, mulh, mulhsu, mulhu, div, divu, rem, remu
      is("b01100".U) {
        out.instructionType := InstructionType.R
      }
      // I: jal
      is("b11011".U) {
        out.instructionType := InstructionType.J
      }
      // I: lb, lh, lw, lbu, lhu
      is("b00000".U) {
        out.instructionType := InstructionType.I
      }
      // I: addi, slti, sltiu, xori, ori, andi, slli, srli, srai
      is("b00100".U) {
        out.instructionType := InstructionType.I
      }
      // I: jalr
      is("b11001".U) {
        out.instructionType := InstructionType.I
      }
      // Zicsr: csrrw, csrrs, csrrc, csrrwi, csrrsi, csrrci
      // I: ecall, ebreak, wfi, sfence.vma
      // M-Level: mret
      // S-Level: sret
      is("b11100".U) {
        out.instructionType := InstructionType.I
      }
      // I: fence
      // Zifencei: fence.i
      is("b00011".U) {
        out.instructionType := InstructionType.I
      }
      // A: lr, sc, amoswap, amoadd, amoxor, amoand, amoor, amomin, amomax, amominu, amomaxu
      is("b01011".U) {
        out.instructionType := InstructionType.R
      }

      // F: fadd.s, fsub.s, fmul.s, fdiv.s, fsqrt.s, fsgnj.s, fsgnjn.s, fsgnjx.s, fmin.s, fmax.s, fcvt.w.s, fcvt.wu.s, fmv.x.w, feq.s, flt.s, fle.s, fclass.s, fcvt.s.w, fcvt.s.wu, fmv.w.x
      // D: fadd.d, fsub.d, fmul.d, fdiv.d, fsqrt.d, fsgnj.d, fsgnjn.d, fsgnjx.d, fmin.d, fmax.d, fcvt.s.d, fcvt.d.s, feq.d, flt.d, fle.d, fclass.d, fcvt.w.d, fcvt.wu.d, fcvt.d.w, fcvt.d.wu
      is("b10100".U) {
        out.instructionType := InstructionType.R
      }

      // F: fmadd.s
      // D: fmadd.d
      is("b10000".U) {
        out.instructionType := InstructionType.R4
      }

      // F: fmsub.s
      // D: fmsub.d
      is("b10001".U) {
        out.instructionType := InstructionType.R4
      }

      // F: fnmsub.s
      // D: fnmsub.d
      is("b10010".U) {
        out.instructionType := InstructionType.R4
      }

      // F: fnmadd.s
      // D: fnmadd.d
      is("b10011".U) {
        out.instructionType := InstructionType.R4
      }

      // F: flw
      // D: fld
      is("b00001".U) {
        out.instructionType := InstructionType.I
      }

      // F: fsw
      // D: fsd
      is("b01001".U) {
        out.instructionType := InstructionType.S
      }
    }

    when(in.instruction(1, 0) =/= "b11".U) { // Identification code detection
      out.instructionType := InstructionType.UNK
    }

    // rd: R/R4/I/U/J
    val fpRd = WireInit(false.B)
    when(in.instruction(6, 2) === "b10100".U) {
      fpRd := (in.instruction(31, 27) in ("b00000".U, "b00001".U, "b00010".U, "b00011".U, "b01011".U, "b00100".U, "b00101".U, "b10100".U, "b10100".U, "b10100".U /* Basic */, "b11010".U /* fcvt.s.w / fcvt.d.w */, "b01000".U /* fcvt.s.d / fcvt.d.s */, "b11110".U /* fmv.w.x */ ))
    }.elsewhen(in.instruction(6, 2) in ("b00001".U, "b10000".U, "b10001".U, "b10010".U, "b10011".U)) {
      fpRd := true.B
    }

    out.rd := new RegisterEntry().zero
    when(out.instructionType in (InstructionType.R, InstructionType.R4, InstructionType.I, InstructionType.U, InstructionType.J)) {
      out.rd.reg := fpRd ## in.instruction(11, 7)
      out.rd.mapping := true.B
    }

    // rs1: R/R4/I/S/B
    val rs1Mapping = WireInit(true.B)
    when(in.instruction(6, 2) === "b11100".U && in.instruction(14)) { // csrrwi, csrrsi, csrrci
      rs1Mapping := false.B
    }

    val fpRs1 = WireInit(false.B)
    when(in.instruction(6, 2) === "b10100".U) {
      fpRs1 := (in.instruction(31, 27) in ("b00000".U, "b00001".U, "b00010".U, "b00011".U, "b01011".U, "b00100".U, "b00101".U, "b10100".U, "b10100".U, "b10100".U /* Basic */, "b11000".U /* fcvt.w.s / fcvt.w.d */, "b11100".U /* fmv.x.w / fclass */, "b01000".U /* fcvt.s.d / fcvt.d.s */ ))
    }.elsewhen(in.instruction(6, 2) in ("b10000".U, "b10001".U, "b10010".U, "b10011".U)) {
      fpRs1 := true.B
    }

    out.rs1 := new RegisterEntry().zero
    when(out.instructionType in (InstructionType.R, InstructionType.R4, InstructionType.I, InstructionType.S, InstructionType.B)) {
      out.rs1.reg := fpRs1 ## in.instruction(19, 15)
      out.rs1.mapping := rs1Mapping
    }

    // rs2: R/R4/S/B
    val rs2Mapping = WireInit(true.B)
    when(in.instruction(6, 2) === "b10100".U && (in.instruction(31, 27) in ("b01000".U, "b11000".U, "b11010".U))) { // fcvt
      rs2Mapping := false.B
    }

    val fpRs2 = WireInit(false.B)
    when(in.instruction(6, 2) in ("b01001".U, "b10000".U, "b10001".U, "b10010".U, "b10011".U, "b10100".U)) {
      fpRs2 := true.B
    }

    out.rs2 := new RegisterEntry().zero
    when(out.instructionType in (InstructionType.R, InstructionType.R4, InstructionType.S, InstructionType.B)) {
      out.rs2.reg := fpRs2 ## in.instruction(24, 20)
      out.rs2.mapping := rs2Mapping
    }

    // rs3: R4
    out.rs3 := new RegisterEntry().zero
    when(out.instructionType in InstructionType.R4) {
      out.rs3.reg := 1.U ## in.instruction(31, 27)
      out.rs3.mapping := true.B
    }

    // func3: R/R4/I/S/B
    out.func3 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.R4, InstructionType.I, InstructionType.S, InstructionType.B)) {
      out.func3 := in.instruction(14, 12)
    }

    // func7: R/R4/I (srli,srai)
    out.func7 := 0.U
    when(out.instructionType in (InstructionType.R, InstructionType.R4, InstructionType.I)) {
      out.func7 := in.instruction(31, 25)
    }

    // imm decode table
    out.imm := MuxLookup(out.instructionType, 0.U)(
      Seq(
        InstructionType.I -> 0.U(20.W) ## in.instruction(31, 20),
        InstructionType.S -> 0.U(20.W) ## in.instruction(31, 25) ## in.instruction(11, 7),
        InstructionType.B -> 0.U(19.W) ## in.instruction(31) ## in.instruction(7) ## in.instruction(30, 25) ## in.instruction(11, 8) ## 0.U,
        InstructionType.U -> in.instruction(31, 12) ## 0.U(12.W),
        InstructionType.J -> 0.U(11.W) ## in.instruction(31) ## in.instruction(19, 12) ## in.instruction(20) ## in.instruction(30, 21) ## 0.U
      )
    )

    // Execute queue arbitration table
    out.executeQueue := MuxCase(
      ExecuteQueueType.alu, // Default ALU
      Seq(
        (in.error =/= MemoryErrorCode.none || out.instructionType === InstructionType.UNK) -> ExecuteQueueType.alu, // Exception handler
        ((out.opcode(6, 2) in ("b11011".U, "b11001".U)) || out.instructionType === InstructionType.B) -> ExecuteQueueType.branch, // jal, jalr, branch
        ((out.opcode(6, 2) in ("b00000".U, "b01011".U, "b00001".U)) || out.instructionType === InstructionType.S) -> ExecuteQueueType.memory, // load, store, lr, sc, amo, flx, fsx
        (out.opcode(6, 2) in ("b10000".U, "b10001".U, "b10010".U, "b10011".U, "b10100".U)) -> ExecuteQueueType.float
      )
    )

    out.pc := in.pc
    out.next := in.next
    out.spec := in.spec
    out.error := in.error
    out.valid := in.valid
  }

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}

/** Register mapping stage
  *
  * Allocate and get renaming registers and ROB
  *
  * Multicycle stage
  */
class RegisterMappingStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec2(new RegisterMappingStageEntry())))
    val out = DecoupledIO(Vec2(new IssueStageEntry()))
    // Mapping interface
    val mapping = new RegisterMappingIO()
    // ROBTable interface
    val robTableWrite = new ROBTableWriteIO()
    // Recovery logic
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = RegInit(Vec2(new RegisterMappingStageEntry()).zero)

  // Waiting for mapping
  io.in.ready := io.mapping.ready && io.out.ready
  when(io.out.fire) { // Stall
    inReg.foreach(_.valid := false.B)
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  io.mapping <> new RegisterMappingIO().zero

  // Mapping logic
  io.mapping.valid := io.out.ready
  inReg.zip(io.mapping.regGroup).foreach { case (in, regGroup) =>
    when(in.valid && in.error === MemoryErrorCode.none) {
      regGroup.rs1 := in.rs1
      regGroup.rs2 := in.rs2
      regGroup.rs3 := in.rs3
      regGroup.rd := in.rd
    }
  }

  // Output logic
  io.out.bits
    .zip(io.robTableWrite.entries)
    .zip(inReg.zip(io.mapping.mappingGroup))
    .foreach { case ((out, robTableWriteEntry), (in, mappingGroup)) =>
      out.opcode := in.opcode
      out.instructionType := in.instructionType
      out.executeQueue := in.executeQueue

      // Mapping result
      out.rs1 := mappingGroup.rs1
      out.rs2 := mappingGroup.rs2
      out.rs3 := mappingGroup.rs3
      out.rd := mappingGroup.rd

      out.func3 := in.func3
      out.func7 := in.func7
      out.imm := in.imm

      out.pc := in.pc
      out.next := in.next
      out.error := in.error
      out.valid := in.valid

      // Write ROB table
      robTableWriteEntry.id := mappingGroup.rd.receipt
      robTableWriteEntry.pc := in.pc
      robTableWriteEntry.rd := Mux(in.rd.mapping, in.rd.reg, 0.U)
      robTableWriteEntry.spec := in.spec
      robTableWriteEntry.valid := in.valid
    }

  // Synchronize with mapping
  io.robTableWrite.wen := io.mapping.valid && io.mapping.ready

  // Waiting for mapping
  io.out.valid := io.mapping.ready

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}

/** Issue stage
  *
  * Instruction queue arbitration
  *
  * Select executable instructions to the instruction queue.
  *
  * When the execution queue types of two instructions are different, two instructions can be issued at once.
  *
  * If two instructions are the same type, will be issued in two cycles.
  *
  * Multicycle stage
  *
  * @param executeQueueWidth
  *   Execute queue width
  */
class IssueStage(executeQueueWidth: Int) extends Module {
  require(executeQueueWidth > 0, "Execute queue depth must be greater than 0")

  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(Vec2(new IssueStageEntry())))
    // Broadcast
    val broadcast = Flipped(new DataBroadcastIO())
    // Execute queue interfaces
    val enqs = Vec(executeQueueWidth, new ExecuteQueueEnqueueIO())
    // Recovery logic
    val recover = Input(Bool())
  })

  private val inReg = RegInit(Vec2(new IssueStageEntry()).zero)

  // Check queue grants
  private val grants = VecInit2(VecInit.fill(executeQueueWidth)(false.B))
  private val queueReady = VecInit2(true.B)

  // Broadcast logic
  for (
    in <- inReg;
    broadcast <- io.broadcast.entries
  ) {
    matchBroadcast(in.rs1, in.rs1, broadcast)
    matchBroadcast(in.rs2, in.rs2, broadcast)
    matchBroadcast(in.rs3, in.rs3, broadcast)
  }

  // Pipeline logic
  queueReady.zip(inReg).foreach { case (ready, in) =>
    when(ready) { // Stall
      in.valid := false.B
    }
  }

  io.in.ready := queueReady(0) && queueReady(1)
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Connect entry to queue
  private def connectQueue(entry: IssueStageEntry, queue: ExecuteQueueEnqueueIO) = {
    queue.enq.bits.opcode := entry.opcode
    queue.enq.bits.instructionType := entry.instructionType

    // Broadcast bypass
    // rs1
    queue.enq.bits.rs1 := entry.rs1
    io.broadcast.entries.foreach { broadcast =>
      matchBroadcast(queue.enq.bits.rs1, entry.rs1, broadcast)
    }
    // rs2
    queue.enq.bits.rs2 := entry.rs2
    io.broadcast.entries.foreach { broadcast =>
      matchBroadcast(queue.enq.bits.rs2, entry.rs2, broadcast)
    }
    // rs3
    queue.enq.bits.rs3 := entry.rs3
    io.broadcast.entries.foreach { broadcast =>
      matchBroadcast(queue.enq.bits.rs3, entry.rs3, broadcast)
    }

    queue.enq.bits.rd := entry.rd
    queue.enq.bits.func3 := entry.func3
    queue.enq.bits.func7 := entry.func7
    queue.enq.bits.imm := entry.imm
    queue.enq.bits.pc := entry.pc
    queue.enq.bits.next := entry.next
    queue.enq.bits.error := entry.error
    queue.enq.bits.valid := entry.valid
  }

  // Grant logic
  io.enqs <> Vec(executeQueueWidth, new ExecuteQueueEnqueueIO()).zero

  // Instruction 0 grant
  for (i <- 0 until executeQueueWidth) {
    grants(0)(i) := inReg(0).executeQueue === io.enqs(i).queueType && inReg(0).valid
    when(grants(0)(i)) {
      queueReady(0) := io.enqs(i).enq.ready
      connectQueue(inReg(0), io.enqs(i))
      io.enqs(i).enq.valid := true.B
    }
  }

  // Instruction 1 grant
  for (i <- 0 until executeQueueWidth) {
    grants(1)(i) := inReg(1).executeQueue === io.enqs(i).queueType && inReg(1).valid && !grants(0)(i) // Priority

    when(grants(1)(i)) {
      queueReady(1) := io.enqs(i).enq.ready
      connectQueue(inReg(1), io.enqs(i))
      io.enqs(i).enq.valid := true.B
    }.elsewhen(inReg(1).executeQueue === io.enqs(i).queueType && inReg(1).valid && grants(0)(i)) { // Conflict
      queueReady(1) := false.B
    }
  }

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
  }
}
