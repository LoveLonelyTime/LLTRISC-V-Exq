package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._

import bergamot.utils.ChiselUtils._
import dataclass.data

/*
 * CSRs (Control and Status Registers) unit, is used to save the core and machine state
 *
 * A CSR is implemented by a register or a functional class
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** CSRs component
  *
  * Provide addressable read and write ports and fixed CSRs output.
  *
  * Responsible for switching states in exceptions and interrupts.
  */
class CSRs extends Module {
  val io = IO(new Bundle {
    // Write interface
    val write = Flipped(new CSRsWriteIO())
    // Read interface
    val read = new CSRsReadIO()
    // Trap interface
    val trap = Flipped(new TrapRequestIO())
    // Fixed CSRs output
    val privilege = Output(PrivilegeType()) // Current core privilege
    val mstatus = Output(DataType.operation)
    val satp = Output(DataType.operation)
    val fcsr = Output(DataType.operation)
    // External drive
    val monitor = Flipped(new MonitorIO())
    val mtime = Input(DataType.double)
    val mtimeIRQ = Input(Bool())
  })

  // Register groups
  private val coreReg = new CoreRegister()
  private val statusReg = new StatusRegister()
  private val exceptionReg = new ExceptionRegister()
  private val interruptsReg = new InterruptsRegister(io.mtimeIRQ, false.B, false.B, false.B)
  private val virtualReg = new VirtualRegister(statusReg.mstatus.value(20), statusReg.privilege)
  private val monitorReg = new MonitorRegister(statusReg.privilege, io.monitor.instret, io.mtime)
  private val pmpReg = new PMPRegister()
  private val fpuReg = new FPURegister()

  // Fixed output
  io.satp := virtualReg.satp.value
  io.privilege := statusReg.privilege
  io.mstatus := statusReg.mstatus.value
  io.fcsr := fpuReg.fcsr.value

  // CSR mapping table

  private val csrMappingTable = Seq(
    // Unprivileged
    "h001".U -> fpuReg.fflags,
    "h002".U -> fpuReg.frm,
    "h003".U -> fpuReg.fcsr,
    "hc00".U -> monitorReg.cycle,
    "hc01".U -> monitorReg.time,
    "hc02".U -> monitorReg.instret,
    "hc80".U -> monitorReg.cycleh,
    "hc81".U -> monitorReg.timeh,
    "hc82".U -> monitorReg.instreth,

    // S-Level
    "h100".U -> statusReg.sstatus,
    "h104".U -> interruptsReg.sie,
    "h105".U -> exceptionReg.stvec,
    "h106".U -> monitorReg.scounteren,
    "h140".U -> exceptionReg.sscratch,
    "h141".U -> exceptionReg.sepc,
    "h142".U -> exceptionReg.scause,
    "h143".U -> exceptionReg.stval,
    "h144".U -> interruptsReg.sip,
    "h180".U -> virtualReg.satp,

    // M-Level
    "h300".U -> statusReg.mstatus,
    "h301".U -> coreReg.misa,
    "h302".U -> exceptionReg.medeleg,
    "h303".U -> interruptsReg.mideleg,
    "h304".U -> interruptsReg.mie,
    "h305".U -> exceptionReg.mtvec,
    "h306".U -> monitorReg.mcounteren,
    "h310".U -> statusReg.mstatush,
    "h320".U -> monitorReg.mcountinhibit,
    "h340".U -> exceptionReg.mscratch,
    "h341".U -> exceptionReg.mepc,
    "h342".U -> exceptionReg.mcause,
    "h343".U -> exceptionReg.mtval,
    "h344".U -> interruptsReg.mip,
    "hb00".U -> monitorReg.mcycle,
    "hb02".U -> monitorReg.minstret,
    "hb80".U -> monitorReg.mcycleh,
    "hb82".U -> monitorReg.minstreth,
    "hf11".U -> coreReg.mvendorid,
    "hf12".U -> coreReg.marchid,
    "hf13".U -> coreReg.mimpid,
    "hf14".U -> coreReg.mhartid,

    // Unsupported but reserved registers
    "h744".U -> ReadAndWriteRegister(() => 0.U) // mnstatus
  ) ++ monitorReg.mhpmcounters ++ // HPM
    monitorReg.mhpmcountersh ++
    monitorReg.mhpmevents ++
    monitorReg.hpmcounters ++
    monitorReg.hpmcountersh ++
    pmpReg.pmps // PMP

  // Read logic
  io.read.error := true.B
  io.read.data := 0.U

  csrMappingTable.foreach { item =>
    when(io.read.address === item._1 && !item._2.guard()) {
      io.read.error := false.B
      io.read.data := item._2.value
    }
  }

  // Write logic
  csrMappingTable.foreach { item =>
    when(io.write.wen && io.write.address === item._1 && !item._2.guard()) {
      item._2.write(io.write.data)
    }
  }

  // CLINT
  io.trap.handlerPC := 0.U
  // Interrupts
  io.trap.interruptPending := false.B
  val pendingInterruptCode = WireInit(DataType.exceptionCode.zeroAsUInt)
  switch(statusReg.privilege) {
    is(PrivilegeType.M) {
      /* Conditions:
       * 1. mie and mip is set
       * 2. mideleg is reset (Do not handle delegated interrupts in M-Level)
       * 3. MIE is set
       */
      val arbitrationList =
        Seq(
          InterruptsCode.machineExternalInterrupt,
          InterruptsCode.machineSoftwareInterrupt,
          InterruptsCode.machineTimerInterrupt,
          InterruptsCode.supervisorExternalInterrupt,
          InterruptsCode.supervisorSoftwareInterrupt,
          InterruptsCode.supervisorTimerInterrupt
        ).map(item => (interruptsReg.mie.value(item) && interruptsReg.mip.value(item) && !interruptsReg.mideleg.value(item) && statusReg.mstatus.value(3)) -> item)

      arbitrationList.reverse.foreach { item =>
        when(item._1) {
          io.trap.interruptPending := true.B
          pendingInterruptCode := item._2.U
        }
      }
    }

    is(PrivilegeType.S) {
      /* Conditions for M-Level interrupts:
       * 1. mie and mip is set (M-Level interrupts cannot be masked)
       *
       * Conditions for S-Level interrupts:
       * 1. sie and sip is set
       * 2. SIE is set
       */
      val machineInterrupts = Seq(
        InterruptsCode.machineExternalInterrupt,
        InterruptsCode.machineSoftwareInterrupt,
        InterruptsCode.machineTimerInterrupt
      )
      val supervisorInterrupts = Seq(
        InterruptsCode.supervisorExternalInterrupt,
        InterruptsCode.supervisorSoftwareInterrupt,
        InterruptsCode.supervisorTimerInterrupt
      )
      val arbitrationList = machineInterrupts.map(item => (interruptsReg.mie.value(item) && interruptsReg.mip.value(item)) -> item) ++
        supervisorInterrupts.map(item => (interruptsReg.mie.value(item) && interruptsReg.mip.value(item) && statusReg.mstatus.value(1)) -> item)

      arbitrationList.reverse.foreach { item =>
        when(item._1) {
          io.trap.interruptPending := true.B
          pendingInterruptCode := item._2.U
        }
      }
    }

    is(PrivilegeType.U) {
      // Any interrupts cannot be masked
      val arbitrationList =
        Seq(
          InterruptsCode.machineExternalInterrupt,
          InterruptsCode.machineSoftwareInterrupt,
          InterruptsCode.machineTimerInterrupt,
          InterruptsCode.supervisorExternalInterrupt,
          InterruptsCode.supervisorSoftwareInterrupt,
          InterruptsCode.supervisorTimerInterrupt
        ).map(item => (interruptsReg.mie.value(item) && interruptsReg.mip.value(item)) -> item)

      arbitrationList.reverse.foreach { item =>
        when(item._1) {
          io.trap.interruptPending := true.B
          pendingInterruptCode := item._2.U
        }
      }
    }
  }

  when(io.trap.interruptTrigger) {
    val delegation = interruptsReg.mideleg.value(pendingInterruptCode)
    printf("Pending code = %d\n", pendingInterruptCode)
    when(!delegation) { // M-Handler
      io.trap.handlerPC := exceptionReg.interruptMLevel(io.trap.trapPC, pendingInterruptCode, io.trap.trapVal)
      statusReg.trapToMLevel()
    }.otherwise { // S-Handler
      io.trap.handlerPC := exceptionReg.interruptSLevel(io.trap.trapPC, pendingInterruptCode, io.trap.trapVal)
      statusReg.trapToSLevel()
    }
  }

  // xret
  when(io.trap.mret) { // mret
    io.trap.handlerPC := exceptionReg.mepc.value
    statusReg.returnFromMLevel()
  }.elsewhen(io.trap.sret) { // sret
    io.trap.handlerPC := exceptionReg.sepc.value
    statusReg.returnFromSLevel()
  }

  // Exception
  when(io.trap.exceptionTrigger) {
    // Delegation check
    val delegation = exceptionReg.medeleg.value(io.trap.trapCode)

    // Exceptions at M-Level will never be delegated to S-Level
    when(statusReg.privilege === PrivilegeType.M || !delegation) { // M-Handler
      io.trap.handlerPC := exceptionReg.exceptionMLevel(io.trap.trapPC, io.trap.trapCode, io.trap.trapVal)
      statusReg.trapToMLevel()
    }.otherwise { // S-Handler
      io.trap.handlerPC := exceptionReg.exceptionSLevel(io.trap.trapPC, io.trap.trapCode, io.trap.trapVal)
      statusReg.trapToSLevel()
    }
  }
}

/** Interrupts registers
  *
  * Include: mie, mip, sie, sip, mideleg
  *
  * @param MTIPFlag
  *   MTIP
  * @param MEIPFlag
  *   MEIP
  * @param STIPFlag
  *   STIP
  * @param SEIPFlag
  *   SEIP
  */
class InterruptsRegister(
    MTIPFlag: Bool,
    MEIPFlag: Bool,
    STIPFlag: Bool,
    SEIPFlag: Bool
) {
  private val MSIP = 0.U // Single HART, not implemented
  private val MTIP = MTIPFlag // Hardwired
  private val MEIP = MEIPFlag // Hardwired
  private val SSIP = RegInit(false.B) // Program register
  private val STIPReg = RegInit(false.B) // Program register
  private val SEIPReg = RegInit(false.B) // Program register
  private val STIP = STIPReg || STIPFlag
  private val SEIP = SEIPReg || SEIPFlag

  private val MSIE = 0.U
  private val MTIE = RegInit(false.B)
  private val MEIE = RegInit(false.B)
  private val SSIE = RegInit(false.B)
  private val STIE = RegInit(false.B)
  private val SEIE = RegInit(false.B)

  private val midelegReg = RegInit(DataType.operation.zeroAsUInt)

  // View
  val mie = ReadAndWriteRegister(
    () => 0.U(20.W) ## MEIE ## 0.U ## SEIE ## 0.U ## MTIE ## 0.U ## STIE ## 0.U ## MSIE ## 0.U ## SSIE ## 0.U,
    data => {
      SSIE := data(1)
      STIE := data(5)
      MTIE := data(7)
      SEIE := data(9)
      MEIE := data(11)
    }
  )

  val mip = ReadAndWriteRegister(
    () => 0.U(20.W) ## MEIP ## 0.U ## SEIP ## 0.U ## MTIP ## 0.U ## STIP ## 0.U ## MSIP ## 0.U ## SSIP ## 0.U,
    data => {
      SSIP := data(1)
      STIPReg := data(5)
      SEIPReg := data(9)
    }
  )

  val sie = ReadAndWriteRegister(
    () => 0.U(22.W) ## SEIE ## 0.U(3.W) ## STIE ## 0.U(3.W) ## SSIE ## 0.U,
    data => {
      SSIE := data(1)
      STIE := data(5)
      SEIE := data(9)
    }
  )

  val sip = ReadAndWriteRegister(
    () => 0.U(22.W) ## SEIP ## 0.U(3.W) ## STIP ## 0.U(3.W) ## SSIP ## 0.U,
    data => {
      SSIP := data(1)
      STIPReg := data(5)
      SEIPReg := data(9)
    }
  )

  val mideleg = ReadAndWriteRegister(
    () => midelegReg,
    data => midelegReg := 0.U(20.W) ## data(11) ## 0.U ## data(9) ## 0.U ## data(7) ## 0.U ## data(5) ## 0.U ## data(3) ## 0.U ## data(1) ## 0.U
  )
}

/** Virtual registers
  *
  * Include: satp
  *
  * @param TVM
  *   TVM
  * @param privilege
  *   Privilege
  */
class VirtualRegister(
    TVM: Bool,
    privilege: PrivilegeType.Type
) {
  private val satpReg = RegInit(DataType.operation.zeroAsUInt)

  val satp = ReadAndWriteRegister(
    () => satpReg,
    data => satpReg := data,
    () => TVM && privilege === PrivilegeType.S
  )
}

/** Exception registers
  *
  * Include: mtvec, medeleg, mepc, mcause, mtval, mscratch, stvec, sepc, scause, stval, sscratch, time, timeh, instret, instreth,
  */
class ExceptionRegister {
  private val mtvecReg = RegInit(DataType.operation.zeroAsUInt)
  private val medelegReg = RegInit(DataType.operation.zeroAsUInt)
  private val mepcReg = RegInit(DataType.operation.zeroAsUInt)
  private val mcauseReg = RegInit(DataType.operation.zeroAsUInt)
  private val mtvalReg = RegInit(DataType.operation.zeroAsUInt)
  private val mscratchReg = RegInit(DataType.operation.zeroAsUInt)

  private val stvecReg = RegInit(DataType.operation.zeroAsUInt)
  private val sepcReg = RegInit(DataType.operation.zeroAsUInt)
  private val scauseReg = RegInit(DataType.operation.zeroAsUInt)
  private val stvalReg = RegInit(DataType.operation.zeroAsUInt)
  private val sscratchReg = RegInit(DataType.operation.zeroAsUInt)

  val mtvec = ReadAndWriteRegister(
    () => mtvecReg,
    data => mtvecReg := data(31, 2) ## 0.U ## data(0)
  )

  val medeleg = ReadAndWriteRegister(
    () => medelegReg,
    data => medelegReg := data
  )

  val mepc = ReadAndWriteRegister(
    () => mepcReg,
    data => mepcReg := data(31, 1) ## 0.U
  )

  val mcause = ReadAndWriteRegister(
    () => mcauseReg,
    data => mcauseReg := data
  )

  val mtval = ReadAndWriteRegister(
    () => mtvalReg,
    data => mtvalReg := data
  )

  val mscratch = ReadAndWriteRegister(
    () => mscratchReg,
    data => mscratchReg := data
  )

  val stvec = ReadAndWriteRegister(
    () => stvecReg,
    data => stvecReg := data(31, 2) ## 0.U ## data(0)
  )

  val sepc = ReadAndWriteRegister(
    () => sepcReg,
    data => sepcReg := data(31, 1) ## 0.U
  )

  val scause = ReadAndWriteRegister(
    () => scauseReg,
    data => scauseReg := data
  )

  val stval = ReadAndWriteRegister(
    () => stvalReg,
    data => stvalReg := data
  )

  val sscratch = ReadAndWriteRegister(
    () => sscratchReg,
    data => sscratchReg := data
  )

  def exceptionMLevel(exceptionPC: UInt, exceptionCode: UInt, exceptionVal: UInt) = {
    mepcReg := exceptionPC
    mcauseReg := 0.U ## 0.U(26.W) ## exceptionCode
    mtvalReg := exceptionVal
    mtvecReg(31, 2) ## 0.U(2.W) // Return handler
  }

  def exceptionSLevel(exceptionPC: UInt, exceptionCode: UInt, exceptionVal: UInt) = {
    sepcReg := exceptionPC
    scauseReg := 0.U ## 0.U(26.W) ## exceptionCode
    stvalReg := exceptionVal
    stvecReg(31, 2) ## 0.U(2.W) // Return handler
  }

  def interruptMLevel(interruptPC: UInt, intervruptCode: UInt, interruptVal: UInt) = {
    mepcReg := interruptPC
    mcauseReg := 1.U ## 0.U(26.W) ## intervruptCode
    mtvalReg := interruptVal

    // Return handler
    Mux(mtvecReg(0), (mtvecReg(31, 2) ## 0.U(2.W)) + (intervruptCode ## 0.U(2.W)), mtvecReg(31, 2) ## 0.U(2.W))
  }

  def interruptSLevel(interruptPC: UInt, intervruptCode: UInt, interruptVal: UInt) = {
    sepcReg := interruptPC
    scauseReg := 1.U ## 0.U(26.W) ## intervruptCode
    stvalReg := interruptVal

    // Return handler
    Mux(stvecReg(0), (stvecReg(31, 2) ## 0.U(2.W)) + (intervruptCode ## 0.U(2.W)), stvecReg(31, 2) ## 0.U(2.W))
  }
}

/** Status registers
  *
  * Include: mstatus, mstatush, sstatus
  */
class StatusRegister {
  private val mstatusReg = RegInit(DataType.operation.zeroAsUInt)
  private val mstatushReg = RegInit(DataType.operation.zeroAsUInt)
  private val privilegeReg = RegInit(PrivilegeType.M)

  val privilege = privilegeReg

  val mstatus = ReadAndWriteRegister(
    () => mstatusReg,
    data => mstatusReg := data(31) ## 0.U(8.W) ## data(22, 7) ## 0.U ## data(5) ## 0.U ## data(3) ## 0.U ## data(1) ## 0.U
  )

  val mstatush = ReadAndWriteRegister(() => mstatushReg)

  val sstatus = ReadAndWriteRegister(
    () =>
      mstatusReg(31) ## 0.U(11.W) ## mstatusReg(19, 18) ## 0.U ## mstatusReg(16, 13) ## 0.U(2.W) ##
        mstatusReg(10, 9) ## mstatusReg(8) ## 0.U ## mstatusReg(6, 5) ## 0.U(3.W) ## mstatusReg(1) ## 0.U,
    data => mstatusReg := data(31) ## 0.U(8.W) ## mstatusReg(22, 20) ## data(19, 18) ## mstatusReg(17) ## data(16, 13) ## mstatusReg(12, 11) ## data(10, 8) ## mstatusReg(7) ## 0.U ## data(5) ## 0.U ## mstatusReg(3) ## 0.U ## data(1) ## 0.U
  )

  // Trap functions
  def trapToMLevel() = {
    privilegeReg := PrivilegeType.M

    val mpp = MuxLookup(privilegeReg, "b00".U)(
      Seq(
        PrivilegeType.M -> "b11".U,
        PrivilegeType.S -> "b01".U,
        PrivilegeType.U -> "b00".U
      )
    )
    val mpie = mstatusReg(3)
    val mie = 0.U
    mstatusReg := mstatusReg(31, 13) ## mpp ## mstatusReg(10, 8) ## mpie ## mstatusReg(6, 4) ## mie ## mstatusReg(2, 0)
  }

  def trapToSLevel() = {
    privilegeReg := PrivilegeType.S

    val spp = MuxLookup(privilegeReg, 0.U)(
      Seq(
        PrivilegeType.S -> 1.U,
        PrivilegeType.U -> 0.U
      )
    )
    val spie = mstatusReg(1)
    val sie = 0.U

    mstatusReg := mstatusReg(31, 9) ## spp ## mstatusReg(7, 6) ## spie ## mstatusReg(4, 2) ## sie ## mstatusReg(0)
  }

  def returnFromMLevel() = {
    val mprv = WireInit(mstatusReg(17))
    switch(mstatusReg(12, 11)) { // mpp
      is("b11".U) { privilegeReg := PrivilegeType.M }
      is("b01".U) {
        privilegeReg := PrivilegeType.S
        mprv := 0.U
      }
      is("b00".U) {
        privilegeReg := PrivilegeType.U
        mprv := 0.U
      }
    }

    mstatusReg := mstatusReg(31, 18) ## mprv ## mstatusReg(16, 13) ## 0.U(2.W) ## mstatusReg(10, 8) ## 1.U ## mstatusReg(6, 4) ## mstatusReg(7) ## mstatusReg(2, 0)
  }

  def returnFromSLevel() = {
    privilegeReg := Mux(mstatusReg(8), PrivilegeType.S, PrivilegeType.U) // spp

    mstatusReg := mstatusReg(31, 18) ## 0.U ## mstatusReg(16, 9) ## 0.U ## mstatusReg(7, 6) ## 1.U ## mstatusReg(4, 2) ## mstatusReg(5) ## mstatusReg(0)
  }
}

/** Core registers
  *
  * Include: misa, mvendorid, mimpid, mhartid
  */
class CoreRegister {
  /*
   * XLEN = 32
   * Atomic extension
   * Compressed extension
   * RV32I/64I/128I base ISA
   * Integer Multiply/Divide extension
   * Supervisor mode implemented
   * User mode implemented
   */
  val misa = ReadAndWriteRegister(() => "b01".U(2.W) ## 0.U(4.W) ## "b00000101000001000100000101".U(26.W))

  val mvendorid = ReadAndWriteRegister(() => 0.U)

  val marchid = ReadAndWriteRegister(() => 0.U)

  // GEN 1
  val mimpid = ReadAndWriteRegister(() => 1.U)

  val mhartid = ReadAndWriteRegister(() => 0.U)
}

/** PMP registers
  *
  * Include: pmpcfg0-15, pmpaddr0-63
  *
  * @note
  *   Unsupported
  */
class PMPRegister {
  // PMP: Unsupported
  private val PMP_START = 0x3a0
  private val PMP_END = 0x3ef
  val pmps = for (i <- 0x3a0 until PMP_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U)
  }
}

/** Monitor registers
  *
  * Include: mcycle, mcycleh, minstret, minstreth, mhpmcounters, mhpmcountersh, mhpmevents, mcountinhibit, mcounteren, scounteren, cycle, cycleh, hpmcounters, hpmcountersh
  *
  * @param privilege
  *   Privilege
  * @param instretVal
  *   The value of instruction retired
  * @param mtime
  *   mtime
  *
  * @note
  *   HPM is unsupported
  */
class MonitorRegister(privilege: PrivilegeType.Type, instretVal: UInt, mtime: UInt) {
  private val mcountinhibitReg = RegInit(DataType.operation.zeroAsUInt)
  private val mcounterenReg = RegInit(DataType.operation.zeroAsUInt)
  private val scounterenReg = RegInit(DataType.operation.zeroAsUInt)

  private val cycleCounter: UInt = RegInit(0.U(64.W))
  when(!mcountinhibitReg(1)) {
    cycleCounter := cycleCounter + 1.U
  }

  private val instretCounter: UInt = RegInit(0.U(64.W))
  when(!mcountinhibitReg(2)) {
    instretCounter + instretVal
  }

  val mcycle = ReadAndWriteRegister(() => cycleCounter(31, 0), data => cycleCounter := cycleCounter(63, 32) ## data)
  val mcycleh = ReadAndWriteRegister(() => cycleCounter(63, 32), data => cycleCounter := data ## cycleCounter(31, 0))

  val minstret = ReadAndWriteRegister(() => instretCounter(31, 0), data => instretCounter := instretCounter(63, 32) ## data)
  val minstreth = ReadAndWriteRegister(() => instretCounter(63, 32), data => instretCounter := data ## instretCounter(31, 0))

  // HPM: Unsupported
  private val MHPMC_START = 0xb03
  private val MHPMC_END = 0xb1f
  private val MHPMCH_START = 0xb83
  private val MHPMCH_END = 0xb9f
  val mhpmcounters = for (i <- MHPMC_START until MHPMC_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U)
  }
  val mhpmcountersh = for (i <- MHPMCH_START until MHPMCH_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U)
  }

  private val MHPME_START = 0x323
  private val MHPME_END = 0x33f
  val mhpmevents = for (i <- MHPME_START until MHPME_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U)
  }

  val mcountinhibit = ReadAndWriteRegister(() => mcountinhibitReg, data => mcountinhibitReg := 0.U(29.W) ## data(2) ## 0.U ## data(0))
  val mcounteren = ReadAndWriteRegister(() => mcounterenReg, data => mcounterenReg := 0.U(29.W) ## data(2, 0))
  val scounteren = ReadAndWriteRegister(() => scounterenReg, data => scounterenReg := 0.U(29.W) ## data(2, 0))

  private def guard(bit: Int) =
    (privilege === PrivilegeType.S && !mcounterenReg(bit)) ||
      (privilege === PrivilegeType.U && (!mcounterenReg(bit) || !scounterenReg(bit)))

  // Read-only
  val cycle = ReadAndWriteRegister(() => cycleCounter(31, 0), _ => (), () => guard(0))
  val cycleh = ReadAndWriteRegister(() => cycleCounter(63, 32), _ => (), () => guard(0))

  val time = ReadAndWriteRegister(() => mtime(31, 0), _ => (), () => guard(1))
  val timeh = ReadAndWriteRegister(() => mtime(63, 32), _ => (), () => guard(1))

  val instret = ReadAndWriteRegister(() => instretCounter(31, 0), _ => (), () => guard(2))
  val instreth = ReadAndWriteRegister(() => instretCounter(63, 32), _ => (), () => guard(2))

  private val HPMC_START = 0xc03
  private val HPMC_END = 0xc1f
  private val HPMCH_START = 0xc83
  private val HPMCH_END = 0xc9f
  val hpmcounters = for (i <- HPMC_START until HPMC_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U, _ => (), () => true.B)
  }
  val hpmcountersh = for (i <- HPMCH_START until HPMCH_END) yield {
    i.U -> ReadAndWriteRegister(() => 0.U, _ => (), () => true.B)
  }
}

/** FPU registers
  *
  * Include: fcsr, frm, fflags
  */
class FPURegister {
  private val fcsrReg = RegInit(DataType.operation.zeroAsUInt)

  val fcsr = ReadAndWriteRegister(() => fcsrReg, data => fcsrReg := data(7, 0))
  val frm = ReadAndWriteRegister(() => fcsrReg(7, 5), data => fcsrReg := data(2, 0) ## fcsrReg(4, 0))
  val fflags = ReadAndWriteRegister(() => fcsrReg(4, 0), data => fcsrReg := fcsrReg(7, 5) ## data(4, 0))
}
