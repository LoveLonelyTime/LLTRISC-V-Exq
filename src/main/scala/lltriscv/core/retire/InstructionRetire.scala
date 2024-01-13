package lltriscv.core.retire

import chisel3._
import chisel3.util._
import lltriscv.core._
import lltriscv.core.record.ROBTableRetireIO
import lltriscv.core.record.RegisterUpdateIO

class InstructionRetire(depth: Int) extends Module {
  val io = IO(new Bundle {
    val retired = Flipped(DecoupledIO(DataType.receiptType))
    val tableRetire = Flipped(new ROBTableRetireIO(depth))

    val update = new RegisterUpdateIO()
    val recover = Output(new Bool())
    val correctPC = Output(DataType.pcType)
  })

  private val id1 = io.retired.bits(30, 0) ## 0.U
  private val id2 = io.retired.bits(30, 0) ## 1.U

  private val retire1 =
    io.tableRetire.entries(id1).commit || !io.tableRetire.entries(id1).valid

  private val retire2 =
    io.tableRetire.entries(id2).commit || !io.tableRetire.entries(id2).valid

  io.recover := false.B
  io.correctPC := 0.U
  io.update.entries.foreach(item => {
    item.rd := 0.U
    item.result := 0.U
  })

  io.retired.ready := false.B
  when(io.retired.valid && retire1 && retire2) {
    io.retired.ready := true.B
    when(
      io.tableRetire.entries(id1).valid || io.tableRetire.entries(id2).valid
    ) {
      printf(
        "retired instruction: \n pc = %d , r = %d, v = %d \n pc = %d , r = %d, v = %d \n",
        io.tableRetire.entries(id1).pc,
        io.tableRetire.entries(id1).result,
        io.tableRetire.entries(id1).valid,
        io.tableRetire.entries(id2).pc,
        io.tableRetire.entries(id2).result,
        io.tableRetire.entries(id2).valid
      )

      io.update.entries(0).rd := io.tableRetire.entries(id1).rd
      io.update.entries(0).result := io.tableRetire.entries(id1).result
      io.update.entries(1).rd := io.tableRetire.entries(id2).rd
      io.update.entries(1).result := io.tableRetire.entries(id2).result

      val id1Violate = io.tableRetire.entries(id1).valid &&
        io.tableRetire.entries(id1).real =/= io.tableRetire.entries(id1).spec
      when(id1Violate) {
        io.recover := true.B
        io.update.entries(1).rd := 0.U // Drop 1
        io.correctPC := io.tableRetire.entries(id1).real
        printf(
          "spec violate!!!: pc = %d, sepc = %d, real = %d\n",
          io.tableRetire.entries(id1).pc,
          io.tableRetire.entries(id1).spec,
          io.tableRetire.entries(id1).real
        )
      }

      when(
        !id1Violate &&
          io.tableRetire.entries(id2).valid &&
          io.tableRetire.entries(id2).real =/= io.tableRetire.entries(id2).spec
      ) {
        io.recover := true.B
        io.correctPC := io.tableRetire.entries(id2).real
        printf(
          "spec violate!!!: pc = %d, sepc = %d, real = %d\n",
          io.tableRetire.entries(id2).pc,
          io.tableRetire.entries(id2).spec,
          io.tableRetire.entries(id2).real
        )
      }
    }
  }
}