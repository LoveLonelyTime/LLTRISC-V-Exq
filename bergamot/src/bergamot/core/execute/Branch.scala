package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.decode.InstructionType
import bergamot.core.record.ExceptionCode

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * Branch processing unit, which is suitable for branch operations
 *
 * Separate processing of branch to accelerate the core speed of branch instructions.
 *
 * List of supported instructions:
 * - I: jal, jalr, beq, bne, blt, bge, bltu, bgeu
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Branch components
  *
  * BranchDecodeStage -> BranchExecuteStage
  */
class Branch extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Recovery interface
    val recover = Input(Bool())
  })
  private val branchDecodeStage = Module(new BranchDecodeStage())
  private val branchExecuteStage = Module(new BranchExecuteStage())

  io.in <> branchDecodeStage.io.in
  branchDecodeStage.io.out <> branchExecuteStage.io.in
  branchExecuteStage.io.out <> io.out

  branchDecodeStage.io.recover := io.recover
  branchExecuteStage.io.recover := io.recover
}

/** Branch decode stage
  *
  * Identify comparison types and comparison operands
  *
  * Single cycle stage
  */
class BranchDecodeStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new ExecuteEntry()))
    val out = DecoupledIO(new BranchExecuteStageEntry())
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
  // op
  io.out.bits.op := BranchOperationType.undefined
  when(inReg.opcode(2)) { // jal & jalr
    io.out.bits.op := BranchOperationType.jal
  }.otherwise { // branch
    io.out.bits.op := MuxLookup(inReg.func3, BranchOperationType.undefined)(
      Seq(
        "b000".U -> BranchOperationType.eq,
        "b001".U -> BranchOperationType.ne,
        "b100".U -> BranchOperationType.lt,
        "b101".U -> BranchOperationType.ge,
        "b110".U -> BranchOperationType.ltu,
        "b111".U -> BranchOperationType.geu
      )
    )
  }

  // op1 & op2
  io.out.bits.op1 := inReg.rs1.receipt
  io.out.bits.op2 := inReg.rs2.receipt

  // add1
  when(inReg.instructionType === InstructionType.I) { // jalr
    io.out.bits.add1 := inReg.rs1.receipt
  }.otherwise { // PC jump
    io.out.bits.add1 := inReg.pc
  }

  // add2
  when(inReg.instructionType === InstructionType.J) { // jal
    io.out.bits.add2 := signExtended(inReg.imm, 20)
  }.elsewhen(inReg.instructionType === InstructionType.I) { // jalr
    io.out.bits.add2 := signExtended(inReg.imm, 11)
  }.otherwise { // branch
    io.out.bits.add2 := signExtended(inReg.imm, 12)
  }

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

/** Branch execute stage
  *
  * Compare and calculate jump addresses
  *
  * Single cycle stage
  */
class BranchExecuteStage extends Module {
  val io = IO(new Bundle {
    // Pipeline interface
    val in = Flipped(DecoupledIO(new BranchExecuteStageEntry()))
    val out = DecoupledIO(new ExecuteResultEntry())
    // Recovery interface
    val recover = Input(Bool())
  })

  // Pipeline logic
  private val inReg = RegInit(new BranchExecuteStageEntry().zero)

  io.in.ready := io.out.ready

  when(io.out.fire) { // Stall
    inReg.valid := false.B
  }
  when(io.in.fire) { // Sample
    inReg := io.in.bits
  }

  // Compare logic
  private val gtu = WireInit(inReg.op1 > inReg.op2)
  private val eq = WireInit(inReg.op1 === inReg.op2)
  private val ne = WireInit(!eq)
  private val ltu = WireInit(!eq && !gtu)
  private val gt = WireInit(false.B)
  private val sign = inReg.op1(CoreConstant.XLEN - 1) ## inReg.op2(CoreConstant.XLEN - 1)
  switch(sign) {
    is("b00".U) { gt := gtu }
    is("b01".U) { gt := true.B }
    is("b10".U) { gt := false.B }
    is("b11".U) { gt := gtu }
  }
  private val lt = WireInit(!eq && !gt)

  private val addPC = WireInit(inReg.add1 + inReg.add2)

  io.out.bits := new ExecuteResultEntry().zero

  // Real PC
  io.out.bits.real := MuxLookup(inReg.op, inReg.next)(
    Seq(
      BranchOperationType.eq -> Mux(eq, addPC, inReg.next),
      BranchOperationType.ne -> Mux(ne, addPC, inReg.next),
      BranchOperationType.ge -> Mux(gt || eq, addPC, inReg.next),
      BranchOperationType.lt -> Mux(lt, addPC, inReg.next),
      BranchOperationType.geu -> Mux(gtu || eq, addPC, inReg.next),
      BranchOperationType.ltu -> Mux(ltu, addPC, inReg.next),
      BranchOperationType.jal -> addPC(CoreConstant.XLEN - 1, 1) ## 0.U // Reset LSB
    )
  )

  // Exception handler
  when(inReg.op === BranchOperationType.undefined) {
    io.out.bits.triggerException(ExceptionCode.illegalInstruction.U)
  }

  io.out.bits.result := inReg.next // Save next PC
  io.out.bits.branch := true.B // This is a branch

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
