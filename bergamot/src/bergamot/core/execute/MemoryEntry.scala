package bergamot.core.execute

import chisel3._
import chisel3.util._

import bergamot.core._

/** Memory operation type
  */
object MemoryOperationType extends ChiselEnum {
  /*
   * Reserve operation:
   * - none: 0
   * - undefined: 0 (Illegal instruction exception)
   *
   * Load operations:
   * - lb: load byte
   * - lh: load half word
   * - lw: load word
   * - ld: load double
   * - lbu: load unsigned byte
   * - lhu: load unsigned half word
   *
   * Store operations:
   * - sb: store byte
   * - sh: store half word
   * - sw: store word
   * - sd: store double
   *
   * AMO operations:
   * - amoswap: read and swap
   * - amoadd: read and add
   * - amoxor: read and xor
   * - amoor: read and or
   * - amoand: read and and
   * - amomax(u): read and max(unsigned)
   * - amomin(u): read and min(unsigned)
   *
   * LR/SC operations:
   * - lr: load reserved
   * - sc: store conditional
   */
  val undefined, none, lb, lh, lw, ld, lbu, lhu, sb, sh, sw, sd, amoswap, amoadd, amoxor, amoand, amoor, amomin, amomax, amominu, amomaxu, lr, sc = Value

  // By type
  val amoValues = List(amoswap, amoadd, amoxor, amoand, amoor, amomin, amomax, amominu, amomaxu)
  val readValues = List(lb, lh, lw, ld, lbu, lhu, lr) ::: amoValues
  val writeValues = List(sb, sh, sw, sd, sc) ::: amoValues

  // By width
  val byteValues = List(lb, lbu, sb)
  val halfValues = List(lh, lhu, sh)
  val wordValues = List(lw, sw, lr, sc) ::: amoValues
  val doubleValues = List(ld, sd)
}

/** Memory access length
  */
object MemoryAccessLength extends ChiselEnum {
  val word, half, byte = Value
}

/** Memory error code
  */
object MemoryErrorCode extends ChiselEnum {
  /*
   * misaligned: Access address is misaligned
   * pageFault: Page table fault
   * memoryFault: Memory fault
   */
  val none, misaligned, pageFault, memoryFault = Value
}

/** Load reservation update interface
  */
class LoadReservationUpdateIO extends Bundle {
  val load = Output(Bool()) // LR or SC?
  val address = Output(DataType.address) // LR address
  val valid = Output(Bool())
}

/** Memory execute stage entry
  *
  * The input entry of MemoryExecuteStage
  */
class MemoryExecuteStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val add1 = DataType.operation // Address addend 1
  val add2 = DataType.operation // Address addend 2
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Memory TLB stage entry
  *
  * The input entry of MemoryTLBStage
  */
class MemoryTLBStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val error = MemoryErrorCode() // Memory error code
  val vaddress = DataType.address // Virtual address
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Memory read write stage entry
  *
  * The input entry of MemoryReadWriteStage
  */
class MemoryReadWriteStageEntry extends Bundle {
  val op = MemoryOperationType() // Memory operation type
  val error = MemoryErrorCode() // Memory error code
  val vaddress = DataType.address // Virtual address
  val paddress = DataType.address // Physical address
  val op1 = DataType.operation // Operand 1
  val rd = DataType.receipt // Destination receipt
  val pc = DataType.address // Corresponding PC
  val next = DataType.address // Next PC
  val valid = Bool() // Validity
}

/** Load reservation entry
  */
class LoadReservationEntry extends Bundle {
  val address = DataType.address
  val valid = Bool()
}
