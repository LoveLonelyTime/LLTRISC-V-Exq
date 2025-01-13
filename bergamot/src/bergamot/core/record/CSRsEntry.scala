package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._

/*
 * CSRs entry
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** Privilege type
  *
  * RISC-V privilege
  */
object PrivilegeType extends ChiselEnum {
  /*
   * M: Machine-Level
   * S: Supervisor-Level
   * U: User-Level
   */
  val M, S, U = Value

  def mcode(code: UInt) = {
    val result = WireInit(U)
    switch(code) {
      is("b00".U) { result := U }
      is("b01".U) { result := S }
      is("b11".U) { result := M }
    }
    result
  }
}

/** Instruction exception code
  *
  * RISC-V instruction exception code
  */
object ExceptionCode {
  val instructionAddressMisaligned = 0
  val instructionAccessFault = 1
  val illegalInstruction = 2
  val breakpoint = 3
  val loadAddressMisaligned = 4
  val loadAccessFault = 5
  val storeAMOAddressMisaligned = 6
  val storeAMOAccessFault = 7
  val environmentCallFromUMode = 8
  val environmentCallFromSMode = 9
  // 10 Reserved
  val environmentCallFromMMode = 11
  val instructionPageFault = 12
  val loadPageFault = 13
  // 14 Reserved
  val storeAMOPageFault = 15
}

/** Interrupts code
  *
  * RISC-V interrupts code
  */
object InterruptsCode {
  // 0 Reserved
  val supervisorSoftwareInterrupt = 1
  // 2 Reserved
  val machineSoftwareInterrupt = 3
  // 4 Reserved
  val supervisorTimerInterrupt = 5
  // 6 Reserved
  val machineTimerInterrupt = 7
  // 8 Reserved
  val supervisorExternalInterrupt = 9
  // 10 Reserved
  val machineExternalInterrupt = 11
}

/** Read and write register
  */
trait ReadAndWriteRegister {

  /** Read CSR register
    *
    * @return
    *   value
    */
  def read(): UInt

  /** Write CSR register
    *
    * @param data
    *   value
    */
  def write(data: UInt): Unit

  /** CSR register guard
    *
    * @return
    *   Access prohibited
    */
  def guard(): Bool

  val value = read()
}

object ReadAndWriteRegister {
  def apply(readFunc: () => UInt): ReadAndWriteRegister = ReadAndWriteRegister(readFunc, data => ())
  def apply(readFunc: () => UInt, writeFunc: (UInt) => Unit): ReadAndWriteRegister = ReadAndWriteRegister(readFunc, writeFunc, () => false.B)
  def apply(readFunc: () => UInt, writeFunc: (UInt) => Unit, guardFunc: () => Bool): ReadAndWriteRegister = new ReadAndWriteRegister {
    def read(): UInt = readFunc()
    def write(data: UInt): Unit = writeFunc(data)
    def guard(): Bool = guardFunc()
  }
}

/** CSRs write intreface
  */
class CSRsWriteIO extends Bundle {
  val address = Output(DataType.csr) // CSR address
  val data = Output(DataType.operation) // Data
  val wen = Output(Bool())
}

/** CSRs read intreface
  */
class CSRsReadIO extends Bundle {
  val address = Input(DataType.csr) // CSR address
  val data = Output(DataType.operation) // Data
  val error = Output(Bool()) // CSR does not exist
}

/** Trap request interface
  */
class TrapRequestIO extends Bundle {
  val mret = Output(Bool()) // M-Level privilege return
  val sret = Output(Bool()) // S-Level privilege return
  val exceptionTrigger = Output(Bool()) // Exception trigger
  val interruptTrigger = Output(Bool()) // Interrupt trigger
  val interruptPending = Input(Bool()) // Interrupt pending flag
  // Exception information
  val trapPC = Output(DataType.address)
  val trapCode = Output(DataType.exceptionCode)
  val trapVal = Output(DataType.operation)
  val handlerPC = Input(DataType.address) // Handler PC
}

/** Monitor interface
  */
class MonitorIO extends Bundle {
  val instret = Output(UInt(2.W))
}
