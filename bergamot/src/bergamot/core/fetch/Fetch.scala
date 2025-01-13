package bergamot.core.fetch

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.decode.DecodeStageEntry
import bergamot.core.record.TLBRequestIO
import bergamot.core.execute.MemoryErrorCode

import bergamot.cache.CacheLineRequestIO

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._
import bergamot.utils.Sv32

/*
 * Instruction fetch
 *
 * Instruction fetch is located behind the instruction cache and is mainly used to control PC logic, instruction validation and branch prediction.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Fetch components
  *
  * InstructionFetcher -> SpeculationStage(InstructionExtender, InstructionPredictor) -> InstructionQueue
  *
  * @param cacheLineDepth
  *   Instruction cache line depth
  * @param cacheCellDepth
  *   Instruction cache cell depth
  * @param queueDepth
  *   Instruction queue depth
  * @param predictorDepth
  *   Branch predictor table depth
  * @param pcInit
  *   The initial value of PC when booting the core
  */
class Fetch(cacheLineDepth: Int, cacheCellDepth: Int, queueDepth: Int, predictorDepth: Int, pcInit: String) extends Module {
  val io = IO(new Bundle {
    val itlb = new TLBRequestIO()
    val icache = new CacheLineRequestIO(cacheCellDepth)
    val out = DecoupledIO(Vec2(new DecodeStageEntry()))
    // Address space ID
    val asid = Input(DataType.asid)
    // Predictor update interface
    val update = Flipped(new BranchPredictorUpdateIO())
    // Correction PC
    val correctPC = Input(DataType.address)
    // Recovery interface
    val recover = Input(Bool())
  })

  private val instructionFetcher = Module(new InstructionFetcher(cacheLineDepth, cacheCellDepth))
  private val speculationStage = Module(new SpeculationStage(predictorDepth, pcInit))
  private val instructionQueue = Module(new InstructionQueue(queueDepth))

  speculationStage.io.in := instructionFetcher.io.out
  instructionFetcher.io.pc := speculationStage.io.pc

  instructionFetcher.io.itlb <> io.itlb
  instructionFetcher.io.icache <> io.icache

  instructionQueue.io.enq <> speculationStage.io.out
  instructionQueue.io.deq <> io.out

  speculationStage.io.asid := io.asid
  speculationStage.io.update <> io.update

  speculationStage.io.correctPC := io.correctPC
  speculationStage.io.recover := io.recover
  instructionQueue.io.recover := io.recover
  instructionFetcher.io.recover := io.recover
}

/** Instruction fetcher
  *
  * Caching two lines at once to connect cross line instructions.
  *
  * Composed of two independent working state machines: ITLB state machine, ICache state machine.
  *
  * ITLB state machine is responsible for caching the PTE corresponding to the current and next cache line.
  *
  * ICache state machine is responsible for caching the current and next cache line.
  *
  * Maximum throughput: 2 32Bits/16Bits instructions / cycle
  *
  * @param cacheLineDepth
  *   Instruction cache line depth
  * @param cacheCellDepth
  *   Instruction cache cell depth
  */
class InstructionFetcher(cacheLineDepth: Int, cacheCellDepth: Int) extends Module {
  require(cacheCellDepth % 2 == 0, "Instruction Cache cell depth must be a multiple of 2")
  require(cacheCellDepth >= 4, "Instruction Cache cell depth must be greater than or equal 4")
  require(cacheCellDepth <= 2048, "Instruction Cache cell depth is too big")

  val io = IO(new Bundle {
    val out = Output(Vec2(new RawInstructionEntry()))
    // The PC of instructions fetching
    val pc = Input(DataType.address)
    // Instruction TLB request interface
    val itlb = new TLBRequestIO()
    // Instruction cache request interface
    val icache = new CacheLineRequestIO(cacheCellDepth)
    val recover = Input(Bool())
  })

  // Buffers
  private val itlbWorkReg = RegInit(Vec2(new ITLBWorkEntry()).zero)
  private val cacheLineWorkReg = RegInit(Vec2(new ICacheLineWorkEntry(cacheCellDepth)).zero)

  // Address helper functions
  private val split = splitCacheLineAddress(cacheLineDepth, cacheCellDepth) _
  private val address = split(io.pc)

  private val alignedCacheLineAddress = address.tag ## address.index ## 0.U((log2Ceil(cacheCellDepth) + 1).W)
  private val alignedNextCacheLineAddress = ((address.tag ## address.index) + 1.U) ## 0.U((log2Ceil(cacheCellDepth) + 1).W)

  // FSM status
  private object Status extends ChiselEnum {
    val idle, request = Value
  }

  /* ---------------- ITLB FSM start ---------------- */

  // Match current ITLB
  private val itlbMatch = VecInit2(false.B)
  itlbMatch.zipWithIndex.foreach { case (item, i) =>
    item := itlbWorkReg(i).valid && itlbWorkReg(i).vpn === Sv32.getVPN(alignedCacheLineAddress)
  }

  // Match next ITLB
  private val nextITLBMatch = VecInit2(false.B)
  nextITLBMatch.zipWithIndex.foreach { case (item, i) =>
    item := itlbWorkReg(i).valid && itlbWorkReg(i).vpn === Sv32.getVPN(alignedNextCacheLineAddress)
  }

  private val itlbStatusReg = RegInit(Status.idle)
  private val itlbQueryAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val itlbVictim = RegInit(0.U) // Victim
  private val itlbTaskFlag = RegInit(false.B) // Undo

  io.itlb <> new TLBRequestIO().zero

  switch(itlbStatusReg) {
    is(Status.idle) {
      when(!itlbMatch.reduceTree(_ || _)) { // ITLB missing
        itlbQueryAddressReg := alignedCacheLineAddress
        itlbVictim := 0.U
        itlbTaskFlag := true.B
        itlbStatusReg := Status.request // Request
      }.elsewhen(!nextITLBMatch.reduceTree(_ || _)) { // Next ITLB missing
        itlbQueryAddressReg := alignedNextCacheLineAddress
        itlbVictim := itlbMatch(0) // Victim is another one
        itlbTaskFlag := true.B
        itlbStatusReg := Status.request // Request
      }
    }

    is(Status.request) {
      io.itlb.vaddress := itlbQueryAddressReg
      io.itlb.valid := true.B
      when(io.itlb.ready) {
        when(itlbTaskFlag) {
          // Not consider 4MiB pages
          itlbWorkReg(itlbVictim).vpn := Sv32.getVPN(itlbQueryAddressReg)
          itlbWorkReg(itlbVictim).ppn := Sv32.get32PPN(io.itlb.paddress)
          itlbWorkReg(itlbVictim).error := io.itlb.error
          itlbWorkReg(itlbVictim).valid := true.B
        }

        itlbStatusReg := Status.idle // Return
      }
    }
  }

  /* ---------------- ITLB FSM end ---------------- */

  /* ---------------- ICache FSM start ---------------- */

  // Match current cache line
  private val cacheLineMatch = VecInit2(false.B)
  cacheLineMatch.zipWithIndex.foreach { case (item, i) =>
    item := cacheLineWorkReg(i).valid && cacheLineWorkReg(i).address === alignedCacheLineAddress
  }

  // Match next cache line
  private val nextCacheLineMatch = VecInit2(false.B)
  nextCacheLineMatch.zipWithIndex.foreach { case (item, i) =>
    item := cacheLineWorkReg(i).valid && cacheLineWorkReg(i).valid && cacheLineWorkReg(i).address === alignedNextCacheLineAddress
  }

  private val iCacheStatusReg = RegInit(Status.idle)
  private val iCacheQueryVAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val iCacheQueryPAddressReg = RegInit(DataType.address.zeroAsUInt)
  private val iCacheVictim = RegInit(0.U)
  private val iCacheTaskFlag = RegInit(false.B)

  io.icache <> new CacheLineRequestIO(cacheCellDepth).zero

  switch(iCacheStatusReg) {
    is(Status.idle) {
      when(!cacheLineMatch.reduceTree(_ || _)) { // Cache line missing
        itlbMatch.zip(itlbWorkReg).foreach { case (matched, item) =>
          when(matched) { // ITLB exists
            when(item.error === MemoryErrorCode.none) { // OK
              iCacheQueryVAddressReg := alignedCacheLineAddress
              iCacheQueryPAddressReg := item.ppn ## Sv32.getOffset(alignedCacheLineAddress)
              iCacheVictim := 0.U
              iCacheTaskFlag := true.B
              iCacheStatusReg := Status.request // Request
            }.otherwise { // ITLB error
              cacheLineWorkReg(0).address := alignedCacheLineAddress
              cacheLineWorkReg(0).error := item.error
              cacheLineWorkReg(0).valid := true.B
            }
          }
        }
      }.elsewhen(!nextCacheLineMatch.reduceTree(_ || _)) { // Next ITLB missing
        val nextVictim = cacheLineMatch(0) // Victim is another one
        nextITLBMatch.zip(itlbWorkReg).foreach { case (matched, item) =>
          when(matched) { // Next ITLB exists
            when(item.error === MemoryErrorCode.none) { // OK
              iCacheQueryVAddressReg := alignedNextCacheLineAddress
              iCacheQueryPAddressReg := item.ppn ## Sv32.getOffset(alignedNextCacheLineAddress)
              iCacheVictim := nextVictim
              iCacheTaskFlag := true.B
              iCacheStatusReg := Status.request // Request
            }.otherwise { // ITLB error
              cacheLineWorkReg(nextVictim).address := alignedNextCacheLineAddress
              cacheLineWorkReg(nextVictim).error := item.error
              cacheLineWorkReg(nextVictim).valid := true.B
            }
          }
        }
      }
    }

    is(Status.request) {
      io.icache.address := iCacheQueryPAddressReg
      io.icache.valid := true.B

      when(io.icache.ready) {
        when(iCacheTaskFlag) {
          cacheLineWorkReg(iCacheVictim).address := iCacheQueryVAddressReg
          cacheLineWorkReg(iCacheVictim).content := io.icache.data
          cacheLineWorkReg(iCacheVictim).error := Mux(io.icache.error, MemoryErrorCode.memoryFault, MemoryErrorCode.none)
          cacheLineWorkReg(iCacheVictim).valid := true.B
        }

        iCacheStatusReg := Status.idle // Return
      }
    }
  }

  /* ---------------- ICache FSM end ---------------- */

  // Merge output logic
  io.out := Vec2(new RawInstructionEntry()).zero

  cacheLineMatch
    .zip(nextCacheLineMatch.reverse)
    .zip(cacheLineWorkReg.zip(cacheLineWorkReg.reverse))
    .foreach { case ((matched, oppositeMatched), (item, opposite)) =>
      when(matched) { // Cache line exists
        // Collect the location of two instructions
        val cachelines = Wire(Vec2(new ICacheLineWorkEntry(cacheCellDepth)))
        val pcValues = Wire(Vec2(DataType.address))
        val offsetValues = pcValues.map(split(_).offset)
        val compress = cachelines.zip(offsetValues).map { case (cacheLine, offset) =>
          isCompressInstruction(cacheLine.content(offset))
        }

        pcValues(0) := io.pc
        cachelines(0) := item
        // The PC of next instruction
        pcValues(1) := pcValues(0) + Mux(compress(0), CoreConstant.compressInstructionLength.U, CoreConstant.instructionLength.U)
        cachelines(1) := Mux(split(pcValues(0)).aligned === split(pcValues(1)).aligned, item, opposite)

        // Output
        io.out.zipWithIndex.foreach { case (out, i) =>
          when(!cachelines(i).valid || cachelines(i).address =/= split(pcValues(i)).aligned) { // Skip
            out.valid := false.B
          }.elsewhen(cachelines(i).error =/= MemoryErrorCode.none) { // Error
            out.error := cachelines(i).error
            out.valid := true.B
          }.elsewhen(compress(i)) { // 16-bits OK
            out.instruction := cachelines(i).content(offsetValues(i))
            out.compress := true.B
            out.valid := true.B
          }.elsewhen(split(pcValues(i)).offset =/= (cacheCellDepth - 1).U) { // Not a boundary, 32-bits OK
            out.instruction := cachelines(i).content(offsetValues(i) + 1.U) ## cachelines(i).content(offsetValues(i))
            out.compress := false.B
            out.valid := true.B
          }.elsewhen(oppositeMatched) { // 32-bits, crossing cache line
            when(opposite.error =/= MemoryErrorCode.none) { // Error
              out.error := opposite.error
              out.valid := true.B
            }.otherwise { // OK
              out.instruction := opposite.content(0) ## cachelines(i).content(offsetValues(i))
              out.compress := false.B
              out.valid := true.B
            }
          }
        }
        // When the first instruction is error, the second instruction cannot be valid
        when(cachelines(0).error =/= MemoryErrorCode.none) {
          io.out(1).valid := false.B
        }
      }
    }

  // Recover logic
  when(io.recover) {
    // Clear buffers
    itlbWorkReg.foreach(_.valid := false.B)
    cacheLineWorkReg.foreach(_.valid := false.B)
    // Undo cache tasks
    itlbTaskFlag := false.B
    iCacheTaskFlag := false.B
  }
}

/** Instruction extender
  *
  * Expanding 16 bits(C Extensions) instructions to 32 bits
  */
class InstructionExtender extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec2(new RawInstructionEntry()))
    val out = Output(Vec2(new RawInstructionEntry()))
  })

  // Extending function
  private def extend(instructionIn: UInt) = {
    val instructionOut = WireInit(instructionIn)
    val imm = WireInit(DataType.immediate.zeroAsUInt)
    val simm = WireInit(DataType.immediate.zeroAsUInt)
    val rs1 = WireInit(DataType.register5.zeroAsUInt)
    val rs2 = WireInit(DataType.register5.zeroAsUInt)
    val rd = WireInit(DataType.register5.zeroAsUInt)

    switch(instructionIn(1, 0)) {
      is("b01".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.nop, c.addi
            imm := instructionIn(12) ## instructionIn(6, 2)
            simm := signExtended(imm, 5)
            instructionOut := simm(11, 0) ## instructionIn(11, 7) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
          }
          is("b001".U) { // c.jal
            imm := instructionIn(12) ## instructionIn(8) ## instructionIn(10, 9) ## instructionIn(6) ## instructionIn(7) ## instructionIn(2) ## instructionIn(11) ## instructionIn(5, 3) ## 0.U
            simm := signExtended(imm, 11)
            instructionOut := simm(20) ## simm(10, 1) ## simm(11) ## simm(19, 12) ## "b00001".U(5.W) ## "b1101111".U(7.W)
          }
          is("b010".U) { // c.li
            imm := instructionIn(12) ## instructionIn(6, 2)
            simm := signExtended(imm, 5)
            instructionOut := simm(11, 0) ## "b00000".U(5.W) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
          }
          is("b011".U) { // c.addi16sp, c.lui
            when(instructionIn(11, 7) === "b00010".U) { // c.addi16sp
              imm := instructionIn(12) ## instructionIn(4, 3) ## instructionIn(5) ## instructionIn(2) ## instructionIn(6) ## "b0000".U(4.W)
              simm := signExtended(imm, 9)
              instructionOut := simm(11, 0) ## instructionIn(11, 7) ## "b000".U(3.W) ## instructionIn(11, 7) ## "b0010011".U(7.W)
            }.otherwise { // c.lui
              imm := instructionIn(12) ## instructionIn(6, 2) ## "b000000000000".U(12.W)
              simm := signExtended(imm, 17)
              instructionOut := simm(31, 12) ## instructionIn(11, 7) ## "b0110111".U(7.W)
            }
          }

          is("b100".U) { // c.srli, c.srai, c.andi, c.sub, c.xor, c.or, c.and
            imm := instructionIn(12) ## instructionIn(6, 2)
            simm := signExtended(imm, 5)
            rs1 := "b01".U ## instructionIn(9, 7)
            rs2 := "b01".U ## instructionIn(4, 2)
            switch(instructionIn(11, 10)) {
              is("b00".U) { // c.srli
                instructionOut := "b0000000".U(7.W) ## imm(4, 0) ## rs1 ## "b101".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b01".U) { // c.srai
                instructionOut := "b0100000".U(7.W) ## imm(4, 0) ## rs1 ## "b101".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b10".U) { // c.andi
                instructionOut := simm(11, 0) ## rs1 ## "b111".U ## rs1 ## "b0010011".U(7.W)
              }
              is("b11".U) { // c.sub, c.xor, c.or, c.and
                switch(instructionIn(6, 5)) {
                  is("b00".U) { // c.sub
                    instructionOut := "b0100000".U(7.W) ## rs2 ## rs1 ## "b000".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b01".U) { // c.xor
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b100".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b10".U) { // c.or
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b110".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                  is("b11".U) { // c.and
                    instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b111".U(3.W) ## rs1 ## "b0110011".U(7.W)
                  }
                }
              }
            }
          }

          is("b101".U) { // c.j
            imm := instructionIn(12) ## instructionIn(8) ## instructionIn(10, 9) ## instructionIn(6) ## instructionIn(7) ## instructionIn(2) ## instructionIn(11) ## instructionIn(5, 3) ## 0.U
            simm := signExtended(imm, 11)
            instructionOut := simm(20) ## simm(10, 1) ## simm(11) ## simm(19, 12) ## "b00000".U(5.W) ## "b1101111".U(7.W)
          }

          is("b110".U) { // c.beqz
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(12) ## instructionIn(6, 5) ## instructionIn(2) ## instructionIn(11, 10) ## instructionIn(4, 3) ## 0.U
            simm := signExtended(imm, 8)
            instructionOut := simm(12) ## simm(10, 5) ## "b00000".U(5.W) ## rs1 ## "b000".U(3.W) ## simm(4, 1) ## simm(11) ## "b1100011".U(7.W)
          }

          is("b111".U) { // c.bnez
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(12) ## instructionIn(6, 5) ## instructionIn(2) ## instructionIn(11, 10) ## instructionIn(4, 3) ## 0.U
            simm := signExtended(imm, 8)
            instructionOut := simm(12) ## simm(10, 5) ## "b00000".U(5.W) ## rs1 ## "b001".U(3.W) ## simm(4, 1) ## simm(11) ## "b1100011".U(7.W)
          }
        }
      }
      is("b10".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.slli
            rs1 := instructionIn(11, 7)
            imm := instructionIn(12) ## instructionIn(6, 2)
            instructionOut := "b0000000".U(7.W) ## imm(4, 0) ## rs1 ## "b001".U(3.W) ## rs1 ## "b0010011".U(7.W)
          }
          is("b010".U) { // c.lwsp
            rd := instructionIn(11, 7)
            imm := instructionIn(3, 2) ## instructionIn(12) ## instructionIn(6, 4) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## "b00010".U(5.W) ## "b010".U(3.W) ## rd ## "b0000011".U(7.W)
          }
          is("b100".U) { // c.jr, c.mv, c.ebreak, c.jalr, c.add
            when(instructionIn(12)) { // c.ebreak, c.jalr, c.add
              rs1 := instructionIn(11, 7)
              rs2 := instructionIn(6, 2)
              when(rs1 === 0.U && rs2 === 0.U) { // c.ebreak
                instructionOut := "b00000000000100000000000001110011".U(32.W)
              }.elsewhen(rs2 === 0.U) { // c.jalr
                instructionOut := "b000000000000".U(12.W) ## rs1 ## "b000".U(3.W) ## "b00001".U(5.W) ## "b1100111".U(7.W)
              }.otherwise { // c.add
                instructionOut := "b0000000".U(7.W) ## rs2 ## rs1 ## "b000".U(3.W) ## rs1 ## "b0110011".U(7.W)
              }
            }.otherwise { // c.jr, c.mv
              when(instructionIn(6, 2) === 0.U) { // c.jr
                rs1 := instructionIn(11, 7)
                instructionOut := "b000000000000".U(12.W) ## rs1 ## "b000".U(3.W) ## "b00000".U(5.W) ## "b1100111".U(7.W)
              }.otherwise { // c.mv
                rd := instructionIn(11, 7)
                rs2 := instructionIn(6, 2)
                instructionOut := "b0000000".U(7.W) ## rs2 ## "b00000".U(5.W) ## "b000".U(3.W) ## rd ## "b0110011".U(7.W)
              }
            }
          }

          is("b110".U) { // c.swsp
            imm := instructionIn(8, 7) ## instructionIn(12, 9) ## "b00".U(2.W)
            rs2 := instructionIn(6, 2)
            instructionOut := imm(11, 5) ## rs2 ## "b00010".U(5.W) ## "b010".U(3.W) ## imm(4, 0) ## "b0100011".U(7.W)
          }
        }
      }
      is("b00".U) {
        switch(instructionIn(15, 13)) {
          is("b000".U) { // c.addi4spn
            rs1 := "b01".U ## instructionIn(4, 2)
            imm := instructionIn(10, 7) ## instructionIn(12, 11) ## instructionIn(5) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## "b00010".U(5.W) ## "b000".U(3.W) ## rs1 ## "b0010011".U(7.W)
            when(instructionIn(12, 5) === 0.U) { instructionOut := 0.U } // nzuimm
          }
          is("b010".U) { // c.lw
            rd := "b01".U ## instructionIn(4, 2)
            rs1 := "b01".U ## instructionIn(9, 7)
            imm := instructionIn(5) ## instructionIn(12, 10) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 0) ## rs1 ## "b010".U(3.W) ## rd ## "b0000011".U(7.W)
          }
          is("b110".U) { // c.sw
            rs1 := "b01".U ## instructionIn(9, 7)
            rs2 := "b01".U ## instructionIn(4, 2)
            imm := instructionIn(5) ## instructionIn(12, 10) ## instructionIn(6) ## "b00".U(2.W)
            instructionOut := imm(11, 5) ## rs2 ## rs1 ## "b010".U(3.W) ## imm(4, 0) ## "b0100011".U(7.W)
          }
        }
      }
    }
    instructionOut // Return
  }

  // Output
  io.out.zip(io.in).foreach { case (out, in) =>
    out.error := in.error
    out.compress := in.compress
    out.instruction := Mux(in.compress, extend(in.instruction), in.instruction)
    out.valid := in.valid
  }
}

/** Instruction predictor
  *
  * Using predictor FSM for instruction prediction
  *
  * @param depth
  *   Branch predictor table depth
  */
class InstructionPredictor(depth: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec2(new RawInstructionEntry()))
    val out = Output(Vec2(new SpeculativeEntry()))
    // The PC of instructions to be predicted
    val pc = Input(DataType.address)
    // Address space ID
    val asid = Input(DataType.asid)
    // Predicted next PC
    val nextPC = Output(DataType.address)
    // Predictor update interface
    val update = Flipped(new BranchPredictorUpdateIO())
  })

  // Instruction address
  private val pcValues = VecInit(
    io.pc,
    io.pc + Mux(io.in(0).compress, CoreConstant.compressInstructionLength.U, CoreConstant.instructionLength.U)
  )

  // Next instruction address
  private val nextPCValues = VecInit(
    pcValues(1),
    pcValues(1) + Mux(io.in(1).compress, CoreConstant.compressInstructionLength.U, CoreConstant.instructionLength.U)
  )

  // Predictor FSM
  // Here, You can choose different implementations
  private val branchPredictor = Module(new TwoBitsBranchPredictor(depth))
  branchPredictor.io.asid := io.asid
  branchPredictor.io.update <> io.update

  private val specValues = Wire(Vec2(DataType.address))
  specValues.zipWithIndex.foreach { case (spec, i) =>
    branchPredictor.io.request.in(i).pc := pcValues(i)
    branchPredictor.io.request.in(i).compress := io.in(i).compress
    spec := branchPredictor.io.request.out(i)
  }

  // Output
  io.out.zipWithIndex.foreach { case (out, i) =>
    out.instruction := io.in(i).instruction
    out.pc := pcValues(i)
    out.spec := specValues(i)
    out.next := nextPCValues(i)
    out.error := io.in(i).error
    out.valid := io.in(i).valid
  }

  // Next PC
  private val grant = VecInit2(false.B)
  grant(0) := io.in(0).valid
  for (i <- 1 until grant.length) {
    grant(i) := grant(i - 1) && io.in(i).valid && specValues(i - 1) === pcValues(i)
    // Not in the prediction chain
    when(!grant(i)) { io.out(i).valid := false.B }
  }

  io.nextPC := io.pc
  grant.zip(specValues).foreach { case (granted, spec) =>
    when(granted) {
      io.nextPC := spec
    }
  }
}

/** Speculation stage
  *
  * Sample, extend and predict instructions
  *
  * Single cycle stage
  *
  * @param depth
  *   Branch predictor table depth
  * @param pcInit
  *   The initial value of PC when booting the core
  */
class SpeculationStage(depth: Int, pcInit: String) extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec2(new RawInstructionEntry()))
    val out = DecoupledIO(Vec2(new SpeculativeEntry()))
    // Address space ID
    val asid = Input(DataType.asid)
    // Predictor update interface
    val update = Flipped(new BranchPredictorUpdateIO())
    // Current PC
    val pc = Output(DataType.address)
    // Correction PC
    val correctPC = Input(DataType.address)
    // Recovery interface
    val recover = Input(Bool())
  })

  // PC register
  private val pcReg = RegInit(pcInit.U(DataType.address.getWidth.W))

  // Pipeline logic
  private val inReg = RegInit(Vec2(new RawInstructionEntry()).zero)
  private val extender = Module(new InstructionExtender())
  private val predictor = Module(new InstructionPredictor(depth))
  predictor.io.pc := pcReg
  predictor.io.asid := io.asid
  predictor.io.update <> io.update

  when(io.out.fire) { // Sample
    inReg := io.in
    pcReg := predictor.io.nextPC
  }

  inReg <> extender.io.in
  extender.io.out <> predictor.io.in

  // Next PC
  io.pc := predictor.io.nextPC

  // Output
  io.out.valid := true.B
  io.out.bits := predictor.io.out

  // Recovery logic
  when(io.recover) {
    inReg.foreach(_.valid := false.B)
    // Correct PC
    pcReg := io.correctPC
  }
}

/** Instruction queue
  *
  * Buffering instructions, implementing with loop pointer
  *
  * @param depth
  *   Instruction queue depth
  */
class InstructionQueue(depth: Int) extends Module {
  require(depth > 0, "Instruction queue depth must be greater than 0")

  val io = IO(new Bundle {
    val enq = Flipped(DecoupledIO(Vec2(new SpeculativeEntry())))
    val deq = DecoupledIO(Vec2(new DecodeStageEntry()))
    val recover = Input(Bool())
  })

  private val queue = RegInit(Vec(depth, Vec2(new SpeculativeEntry())).zero)

  private val incrRead = WireInit(false.B)
  private val incrWrite = WireInit(false.B)
  private val (readPtr, nextRead) = pointer(depth, incrRead)
  private val (writePtr, nextWrite) = pointer(depth, incrWrite)

  private val emptyReg = RegInit(true.B)
  private val fullReg = RegInit(false.B)

  io.enq.ready := !fullReg
  io.deq.valid := !emptyReg

  // Prevent invalid instructions from blocking queues to accelerate fetching instructions
  private val validEnqueue = VecInit(io.enq.bits.map(_.valid)).reduceTree(_ || _)
  private val op = (io.enq.fire && validEnqueue) ## (io.deq.fire)
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

  io.deq.bits <> queue(readPtr)

  // Write logic
  when(doWrite) {
    queue(writePtr) := io.enq.bits
  }

  // Recovery logic
  when(io.recover) {
    queue.foreach(_.foreach(_.valid := false.B))
  }
}
