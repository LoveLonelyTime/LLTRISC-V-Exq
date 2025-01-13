package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.decode.InstructionType
import bergamot.core.record.CSRsReadIO
import bergamot.core.record.PrivilegeType
import bergamot.core.record.ExceptionCode

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * ALU (Arithmetic and Logic Unit), which is suitable for integer operations
 *
 * List of supported instructions:
 * - I: lui, auipc, add, sub, sll, slt, sltu, xor, srl, sra, or, and, addi, slti, sltiu, xori, ori, andi, slli, srli, srai, ecall, ebreak, fence, wfi
 * - M: mul, mulh, mulhsu, mulhu, div, divu, rem, remu
 * - Zicsr: csrrw, csrrs, csrrc, csrrwi, csrrsi, csrrci
 * - M-Level: mret
 * - S-Level: sret, sfence.vma
 * - Zifencei: fence.i
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** ALU components
  *
  * Execute basic ALU instructions
  *
  * ALUDecodeStage -> ALUExecuteStage
  */
class ALU extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // CSR read interface
    val csr = Flipped(new CSRsReadIO())
    // MStatus
    val mstatus = Input(DataType.operation)
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // Recovery interface
    val recover = Input(Bool())
  })
  private val aluDecodeStage = Module(new ALUDecodeStage())
  private val aluExecuteStage = Module(new ALUExecuteStage())

  io.in <> aluDecodeStage.io.in
  aluDecodeStage.io.out <> aluExecuteStage.io.in
  aluExecuteStage.io.out <> io.out

  // Recovery interface
  aluDecodeStage.io.recover := io.recover
  aluExecuteStage.io.recover := io.recover

  // Privilege interface
  aluDecodeStage.io.csr <> io.csr
  aluDecodeStage.io.privilege := io.privilege
  aluExecuteStage.io.privilege := io.privilege
  aluExecuteStage.io.mstatus := io.mstatus
}

/** ALU decode stage
  *
  * Secondary decoding of instructions: extract information and read CSRs
  *
  * Single cycle stage
  */
class ALUDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ALUExecuteStageEntry())
    // CSR read interface
    val csr = Flipped(new CSRsReadIO())
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // Recovery interface
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
  io.out.bits := new ALUExecuteStageEntry().zero
  io.csr <> new CSRsReadIO().zero

  io.out.bits.op := ALUOperationType.undefined
  // op
  switch(inReg.opcode(6, 2)) {
    is("b00100".U) { // ALU type I instructions
      io.out.bits.op := MuxLookup(inReg.func3, ALUOperationType.undefined)(
        Seq(
          "b000".U -> ALUOperationType.add,
          "b100".U -> ALUOperationType.xor,
          "b110".U -> ALUOperationType.or,
          "b111".U -> ALUOperationType.and,
          "b001".U -> ALUOperationType.sll,
          "b010".U -> ALUOperationType.slt,
          "b011".U -> ALUOperationType.sltu,
          "b101".U -> Mux(inReg.func7(5), ALUOperationType.sra, ALUOperationType.srl)
        )
      )
    }

    is("b01100".U) { // Basic type R instructions
      when(inReg.func7(0)) { // M Extensions
        io.out.bits.op := MuxLookup(inReg.func3, ALUOperationType.undefined)(
          Seq(
            "b000".U -> ALUOperationType.mul,
            "b001".U -> ALUOperationType.mulh,
            "b010".U -> ALUOperationType.mulhsu,
            "b011".U -> ALUOperationType.mulhu,
            "b100".U -> ALUOperationType.div,
            "b101".U -> ALUOperationType.divu,
            "b110".U -> ALUOperationType.rem,
            "b111".U -> ALUOperationType.remu
          )
        )
      }.otherwise { // ALU type R instructions
        io.out.bits.op := MuxLookup(inReg.func3, ALUOperationType.undefined)(
          Seq(
            "b000".U -> Mux(inReg.func7(5), ALUOperationType.sub, ALUOperationType.add),
            "b100".U -> ALUOperationType.xor,
            "b110".U -> ALUOperationType.or,
            "b111".U -> ALUOperationType.and,
            "b001".U -> ALUOperationType.sll,
            "b010".U -> ALUOperationType.slt,
            "b011".U -> ALUOperationType.sltu,
            "b101".U -> Mux(inReg.func7(5), ALUOperationType.sra, ALUOperationType.srl)
          )
        )
      }
    }

    is("b01101".U) { // lui
      io.out.bits.op := ALUOperationType.add
    }
    is("b00101".U) { // auipc
      io.out.bits.op := ALUOperationType.add
    }

    is("b11100".U) { // csr, xret, env, ebreak, wfi, sfence.vma
      io.out.bits.op := MuxLookup(inReg.func3, ALUOperationType.undefined)(
        Seq(
          "b000".U -> MuxCase( // xret, env, ebreak, wfi, sfence.vma
            ALUOperationType.undefined,
            Seq(
              (inReg.imm === "b000000000000".U) -> ALUOperationType.env,
              (inReg.imm === "b000000000001".U) -> ALUOperationType.ebreak,
              (inReg.imm === "b000100000010".U) -> ALUOperationType.sret,
              (inReg.imm === "b001100000010".U) -> ALUOperationType.mret,
              (inReg.imm === "b000100000101".U) -> ALUOperationType.none, // wfi -> nop
              (inReg.imm(11, 5) === "b0001001".U) -> ALUOperationType.sfenceVMA
            )
          ),
          "b001".U -> ALUOperationType.csrrw,
          "b010".U -> ALUOperationType.csrrs,
          "b011".U -> ALUOperationType.csrrc,
          "b101".U -> ALUOperationType.csrrw,
          "b110".U -> ALUOperationType.csrrs,
          "b111".U -> ALUOperationType.csrrc
        )
      )
    }

    is("b00011".U) { // fence, fence.i
      io.out.bits.op := Mux(inReg.func3(0), ALUOperationType.fencei, ALUOperationType.fence)
    }
  }

  // op1 & op2 & csr
  switch(inReg.instructionType) {
    is(InstructionType.R) { // R
      io.out.bits.op1 := inReg.rs1.receipt
      io.out.bits.op2 := inReg.rs2.receipt
    }

    is(InstructionType.I) { // I
      when(inReg.opcode(6, 2) === "b11100".U && inReg.func3 =/= 0.U) { // CSR I
        // CSR access
        io.out.bits.csrAddress := inReg.imm(11, 0)
        io.csr.address := inReg.imm(11, 0)

        io.out.bits.op1 := io.csr.data
        io.out.bits.op2 := Mux(inReg.func3(2), inReg.rs1.origin, inReg.rs1.receipt)

        /*
         * Special protection is in CSRs
         * But, general protection is here
         */
        // Treat csrrs/csrrc 0 as a read operation
        io.out.bits.csrError :=
          io.csr.error || // Special protection
            ((io.out.bits.op2 =/= 0.U || io.out.bits.op === ALUOperationType.csrrw) && inReg.imm(11, 10) === "b11".U) || // Read-only violation
            (io.privilege === PrivilegeType.S && inReg.imm(9, 8) === "b11".U) || (io.privilege === PrivilegeType.U && inReg.imm(9, 8) =/= "b00".U) // Unauthorized access
      }.otherwise { // General I
        io.out.bits.op1 := inReg.rs1.receipt
        io.out.bits.op2 := signExtended(inReg.imm, 11) // Extend imm
      }
    }

    is(InstructionType.U) { // lui / auipc
      io.out.bits.op1 := Mux(inReg.opcode(5), 0.U, inReg.pc)
      io.out.bits.op2 := inReg.imm // Upper
    }
  }

  io.out.bits.rd := inReg.rd.receipt
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.valid := inReg.valid
  io.out.bits.error := inReg.error

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}

/** ALU execute stage
  *
  * Execute ALU operation
  *
  * Single cycle stage
  */
class ALUExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ALUExecuteStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // MStatus
    val mstatus = Input(DataType.operation)
    // Recovery interface
    val recover = Input(Bool())
  })
  // Pipeline logic
  private val inReg = RegInit(new ALUExecuteStageEntry().zero)

  io.in.ready := io.out.ready

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Execute logic
  io.out.bits := new ExecuteResultEntry().zero
  switch(inReg.op) {
    // ALU
    is(ALUOperationType.add) {
      io.out.bits.result := inReg.op1 + inReg.op2
    }
    is(ALUOperationType.sub) {
      io.out.bits.result := inReg.op1 - inReg.op2
    }
    is(ALUOperationType.and) {
      io.out.bits.result := inReg.op1 & inReg.op2
    }
    is(ALUOperationType.or) {
      io.out.bits.result := inReg.op1 | inReg.op2
    }
    is(ALUOperationType.xor) {
      io.out.bits.result := inReg.op1 ^ inReg.op2
    }
    is(ALUOperationType.sll) {
      io.out.bits.result := inReg.op1 << inReg.op2(4, 0)
    }
    is(ALUOperationType.srl) {
      io.out.bits.result := inReg.op1 >> inReg.op2(4, 0)
    }
    is(ALUOperationType.sra) {
      io.out.bits.result := (inReg.op1.asSInt >> inReg.op2(4, 0)).asUInt
    }
    is(ALUOperationType.slt) {
      io.out.bits.result := Mux(inReg.op1.asSInt < inReg.op2.asSInt, 1.U, 0.U)
    }
    is(ALUOperationType.sltu) {
      io.out.bits.result := Mux(inReg.op1 < inReg.op2, 1.U, 0.U)
    }
    is(ALUOperationType.mul) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2.asSInt)(31, 0)
    }
    is(ALUOperationType.mulh) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2.asSInt)(63, 32)
    }
    is(ALUOperationType.mulhsu) {
      io.out.bits.result := (inReg.op1.asSInt * inReg.op2)(63, 32)
    }
    is(ALUOperationType.mulhu) {
      io.out.bits.result := (inReg.op1 * inReg.op2)(63, 32)
    }
    is(ALUOperationType.div) {
      io.out.bits.result := Mux(inReg.op2 === 0.U, "hffffffff".U, (inReg.op1.asSInt / inReg.op2.asSInt).asUInt)
    }
    is(ALUOperationType.divu) {
      io.out.bits.result := Mux(inReg.op2 === 0.U, "hffffffff".U, inReg.op1 / inReg.op2)
    }
    is(ALUOperationType.rem) {
      io.out.bits.result := Mux(inReg.op2 === 0.U, inReg.op1, (inReg.op1.asSInt % inReg.op2.asSInt).asUInt)
    }
    is(ALUOperationType.remu) {
      io.out.bits.result := Mux(inReg.op2 === 0.U, inReg.op1, inReg.op1 % inReg.op2)
    }

    // CSR
    is(ALUOperationType.csrrw) {
      io.out.bits.result := inReg.op1
      io.out.bits.resultCSR(inReg.csrAddress, inReg.op2)
    }
    is(ALUOperationType.csrrs) {
      io.out.bits.result := inReg.op1
      // When inReg.op2 === 0, it is a read operation
      when(inReg.op2 =/= 0.U) { // Avoid unnecessary flushing of the pipeline
        io.out.bits.resultCSR(inReg.csrAddress, inReg.op1 | inReg.op2)
      }
    }
    is(ALUOperationType.csrrc) {
      io.out.bits.result := inReg.op1
      when(inReg.op2 =/= 0.U) {
        io.out.bits.resultCSR(inReg.csrAddress, inReg.op1 & ~inReg.op2)
      }
    }

    // ebreak/env
    is(ALUOperationType.ebreak) {
      io.out.bits.triggerException(ExceptionCode.breakpoint.U)
    }
    is(ALUOperationType.env) {
      switch(io.privilege) {
        is(PrivilegeType.M) { io.out.bits.triggerException(ExceptionCode.environmentCallFromMMode.U) }
        is(PrivilegeType.S) { io.out.bits.triggerException(ExceptionCode.environmentCallFromSMode.U) }
        is(PrivilegeType.U) { io.out.bits.triggerException(ExceptionCode.environmentCallFromUMode.U) }
      }
    }

    // xret
    is(ALUOperationType.sret) {
      when((io.privilege === PrivilegeType.S && !io.mstatus(22)) || io.privilege === PrivilegeType.M) { // TSR OK
        io.out.bits.sret := true.B
      }.otherwise { // Unauthorized access
        io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
      }
    }

    is(ALUOperationType.mret) {
      when(io.privilege === PrivilegeType.M) { // OK
        io.out.bits.mret := true.B
      }.otherwise { // Unauthorized access
        io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
      }
    }

    // fence
    is(ALUOperationType.fence) {
      when(inReg.op2(4)) { // PW
        // Visible to device
        io.out.bits.flushDCache := true.B
        io.out.bits.flushL2DCache := true.B
      }
    }

    is(ALUOperationType.fencei) {
      // Visible to ICache
      io.out.bits.flushDCache := true.B
      io.out.bits.invalidICache := true.B
    }

    is(ALUOperationType.sfenceVMA) {
      when((io.privilege === PrivilegeType.S && !io.mstatus(20)) || io.privilege === PrivilegeType.M) { // TVM
        // Visible to TLB
        io.out.bits.flushDCache := true.B
        io.out.bits.invalidTLB := true.B
      }.otherwise {
        io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
      }
    }

    // Illegal instruction
    is(ALUOperationType.undefined) {
      io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
    }
  }

  // CSR Error
  when(inReg.csrError) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
  }

  // Instruction cache line error
  when(inReg.error === MemoryErrorCode.memoryFault) {
    io.out.bits.triggerException(ExceptionCode.instructionAccessFault.U, inReg.pc)
  }.elsewhen(inReg.error === MemoryErrorCode.pageFault) {
    io.out.bits.triggerException(ExceptionCode.instructionPageFault.U, inReg.pc)
  }

  io.out.bits.rd := inReg.rd
  io.out.bits.pc := inReg.pc
  io.out.bits.next := inReg.next
  io.out.bits.real := inReg.next
  io.out.bits.valid := inReg.valid

  io.out.valid := true.B // No wait

  // Recovery logic
  when(io.recover) {
    inReg.valid := false.B
  }
}
