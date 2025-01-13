package bergamot.core.fetch

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._

/*
 * Branch predictor
 *
 * The branch predictor uses FSM to guess which branch will be run before the end of branch instruction execution,
 * in order to improve the efficiency of the processor's instruction pipeline.
 * The branch predictor consists of BTB(Branch Target Buffer) table entries and FSM algorithm.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Abstract branch predictor
  *
  * Declare the basic interface of the branch predictor
  *
  * @param depth
  *   BTB table depth
  */
abstract class BranchPredictor(depth: Int) extends Module {
  require(depth > 0, "Branch predictor table depth must be greater than 0")

  val io = IO(new Bundle {
    val asid = Input(DataType.asid)
    val request = Flipped(new BranchPredictorRequestIO())
    val update = Flipped(new BranchPredictorUpdateIO())
  })
}

/** TwoBits(4 states) branch predictor
  *
  * A branch predictor with 2-bits state encoding
  *
  * Loop pointer replacement strategy
  *
  * @param depth
  *   BTB table depth, each table entry contains two instruction predictions.
  */
class TwoBitsBranchPredictor(depth: Int) extends BranchPredictor(depth) {
  // History enum
  private object History extends ChiselEnum {
    val NN, NT, TN, TT = Value
  }

  // BTB entry
  private class Entry extends Bundle {
    val asid = DataType.asid
    val pc = DataType.address
    val address = DataType.address
    val history = History()
  }

  private val table = RegInit(Vec(depth, Vec2(new Entry())).zero)
  private val incrVictim = WireInit(false.B)
  private val (victimPtr, nextVictimPtr) = pointer(depth, incrVictim)

  // FSM output table
  private def outputTable(next: UInt, spec: UInt) =
    Seq(
      History.NN -> next,
      History.NT -> spec,
      History.TN -> spec,
      History.TT -> spec
    )

  // FSM transition table
  private def transitionTable(jump: Bool) =
    Seq(
      History.NN -> Mux(jump, History.NT, History.NN),
      History.NT -> Mux(jump, History.TT, History.TN),
      History.TN -> Mux(jump, History.NT, History.NN),
      History.TT -> Mux(jump, History.TT, History.TN)
    )

  // FSM init table
  private def initTable(jump: Bool) = Mux(jump, History.NT, History.NN)

  // Output
  io.request.in.zip(io.request.out).foreach { case (in, out) =>
    // Default
    val nextPC = in.pc + Mux(in.compress, CoreConstant.compressInstructionLength.U, CoreConstant.instructionLength.U)
    out := nextPC

    // Fully-associative
    for (
      entry <- table;
      item <- entry
    ) {
      when(in.pc === item.pc && io.asid === item.asid) {
        out := MuxLookup(item.history, nextPC)(outputTable(nextPC, item.address))
      }
    }
  }

  // Update
  io.update.entries.zipWithIndex.foreach { case (updateEntry, i) =>
    when(updateEntry.valid) {
      val found = WireInit(false.B)

      // Fully-associative
      for (
        entry <- table;
        item <- entry
      ) {
        when(updateEntry.pc === item.pc && io.asid === item.asid) {
          found := true.B
          item.address := updateEntry.address
          item.history := MuxLookup(item.history, History.NN)(transitionTable(updateEntry.jump))
        }
      }

      // Replace
      when(!found) {
        table(victimPtr)(i).address := updateEntry.address
        table(victimPtr)(i).pc := updateEntry.pc
        table(victimPtr)(i).asid := io.asid
        table(victimPtr)(i).history := initTable(updateEntry.jump)
        incrVictim := true.B
      }
    }
  }
}
