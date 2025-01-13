package bergamot.core.fetch

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.CoreUtils._
import bergamot.utils.ChiselUtils._

/*
 * Branch predictor entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Branch predictor request interface
  *
  * 2-ways
  */
class BranchPredictorRequestIO extends Bundle {
  val in = Output(
    Vec2(
      new Bundle {
        val pc = DataType.address
        val compress = Bool()
      }
    )
  )

  val out = Input(Vec2(DataType.address))
}

/** Branch predictor update interface
  *
  * 2-ways
  */
class BranchPredictorUpdateIO extends Bundle {
  val entries = Output(
    Vec2(
      new Bundle {
        val pc = DataType.address // Branch instruction PC
        val address = DataType.address // Address jumped
        val jump = Bool() // Has a jump occurred?
        val valid = Bool()
      }
    )
  )
}
