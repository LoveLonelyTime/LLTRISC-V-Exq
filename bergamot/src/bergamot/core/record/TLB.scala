package bergamot.core.record

import chisel3._
import chisel3.util._

import bergamot.core._
import bergamot.core.execute.MemoryAccessLength
import bergamot.core.execute.MemoryErrorCode

import bergamot.cache.FlushCacheIO

import bergamot.bus.SMAReaderIO

import bergamot.utils.ChiselUtils._
import bergamot.utils.CoreUtils._
import bergamot.utils.Sv32

/*
 * TLB (Translation Lookaside Buffer)
 *
 * TLB performs virtual address translation and cache PTE to accelerate the entire process.
 * Currently, supports the Sv32 specification.
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

/** TLB
  *
  * TLB for accessing to data (load/store) and TLB for accessing to instruction (fetch)
  *
  * @param depth
  *   TLB depth
  * @param data
  *   Data TLB or instruction TLB
  */
class TLB(depth: Int, data: Boolean) extends Module {
  val io = IO(new Bundle {
    // Request interface
    val request = Flipped(new TLBRequestIO())
    // SMA interface
    val sma = new SMAReaderIO()
    // Current core privilege
    val privilege = Input(PrivilegeType())
    // satp
    val satp = Input(DataType.operation)
    // mstatus
    val mstatus = Input(DataType.operation)
    // Flush request interface: Invalidate all entry
    val flush = Flipped(new FlushCacheIO())
  })

  // TLB
  private val table = RegInit(Vec(depth, new TLBEntry()).zero)

  private val statusReg = RegInit(Status.idle)
  private object Status extends ChiselEnum {
    /*
     * idle: Idle
     * lookup: Fully associative search
     * vpn1: PTE walk vpn1
     * vpn0: PTE walk vpn0
     * flush: Flush TLB
     */
    val idle, lookup, vpn1, vpn0, flush = Value
  }

  private val incrVictim = WireInit(false.B)
  private val (victimPtr, nextVictimPtr) = pointer(depth, incrVictim)

  /** Alloc TLB entry
    *
    * @param pte
    *   PTE entry
    * @param mPage
    *   A 4MiB-page PTE entry
    * @param global
    *   Inherited global field
    */
  private def alloc(pte: UInt, mPage: Bool, global: Bool) = {
    // printf("TLB save: %d\n", pte)
    incrVictim := true.B
    val victim = table(victimPtr)
    victim.vpn := io.request.vaddress(31, 12)
    victim.ppn := pte(31, 10)
    victim.da := pte(7, 6)
    victim.g := pte(5) || global
    victim.uxwr := pte(4, 1)
    victim.v := pte(0)
    victim.mPage := mPage
    victim.asid := io.satp(30, 22)
    victim.valid := true.B
  }

  // Empty
  io.flush.empty := !VecInit(table.map(_.valid)).reduceTree(_ || _)

  // Actual paging privilege level (MPRV)
  // Instruction address-translation and protection are unaffected by the setting of MPRV
  private val pagePrivilege =
    if (data)
      Mux(
        io.privilege === PrivilegeType.M,
        Mux(
          io.mstatus(17), // MPRV
          PrivilegeType.mcode(io.mstatus(12, 11)), // MPP
          PrivilegeType.M
        ),
        io.privilege
      )
    else io.privilege

  private def alignmentCheck(entry: TLBEntry) = {
    !entry.mPage || entry.ppn(9, 0) === 0.U
  }

  private def pteDACheck(da: UInt) = {
    da(0) && // A is set
    (!io.request.write || da(1)) // Write and D is set or just read
  }

  private def ptePrivilegeCheck(uxwr: UInt) = {
    val r = Mux(io.mstatus(19), uxwr(0) || uxwr(2), uxwr(0)) // MXR
    val w = uxwr(1)
    val x = uxwr(2)
    val u = uxwr(3)

    // When this is a data TLB, check r/w.
    // Otherwise, an instruction TLB checking x.
    val xwrResult = if (data) Mux(io.request.write, r && w, r) else x
    val suResult = WireInit(false.B)

    // Check S/U
    switch(pagePrivilege) {
      is(PrivilegeType.S) {
        suResult := Mux(io.mstatus(18), true.B, !u) // SUM
      }
      is(PrivilegeType.U) {
        suResult := u
      }
    }

    xwrResult && suResult
  }

  io.request <> new TLBRequestIO().zero

  // Sample
  when(statusReg === Status.idle) {
    when(io.flush.req) { // Flush
      table.foreach(_.valid := false.B)
    }.elsewhen(io.request.valid) {
      // MPRV check
      when(pagePrivilege === PrivilegeType.M || !io.satp(31)) { // Page memory disabled
        io.request.paddress := io.request.vaddress
        io.request.error := MemoryErrorCode.none
        io.request.ready := true.B
      }.otherwise { // Page memory enabled
        statusReg := Status.lookup
      }
    }
  }

  // Lookup
  when(statusReg === Status.lookup) {
    // Fully-associative
    val grant = VecInit.fill(depth)(false.B)
    grant.zip(table).foreach { case (granted, entry) =>
      val matchAddress = Mux(
        entry.mPage,
        Sv32.getVPN1(io.request.vaddress) === entry.vpn(19, 10), // 4MiB page
        Sv32.getVPN(io.request.vaddress) === entry.vpn // 4KiB page
      )

      val matchASID = entry.g || entry.asid === io.satp(30, 22)
      granted := entry.valid && matchAddress && matchASID
    }

    when(grant.reduceTree(_ || _)) { // Hit

      grant.zip(table).foreach { case (granted, entry) =>
        when(granted) {
          // printf("TLB Hit: vaddr= %x, paddr= %x, da= %d, uxwr = %d, v = %d\n", io.request.vaddress, io.request.paddress, entry.da, entry.uxwr, entry.v)
          // printf("Check TLB da = %d, pc = %d, ac = %d\n", pteDACheck(entry.da), ptePrivilegeCheck(entry.uxwr), alignmentCheck(entry))
          when(entry.v && pteDACheck(entry.da) && ptePrivilegeCheck(entry.uxwr) && alignmentCheck(entry)) {
            // 34 -> 32
            io.request.paddress := Mux(
              entry.mPage,
              entry.ppn(21, 10) ## Sv32.getMOffset(io.request.vaddress), // 4MiB page
              entry.ppn ## Sv32.getOffset(io.request.vaddress) // 4KiB page
            )

          }.otherwise { // Fault
            io.request.error := MemoryErrorCode.pageFault
          }
        }
      }

      io.request.ready := true.B
      statusReg := Status.idle
    }.otherwise { // Miss
      statusReg := Status.vpn1
    }
  }

  io.sma <> new SMAReaderIO().zero

  // TLB walk: vpn1
  private val vpn1Reg = RegInit(DataType.operation.zeroAsUInt)
  when(statusReg === Status.vpn1) {
    // PTE-1 entry address (34 -> 32)
    io.sma.address := io.satp(21, 0) ## Sv32.getVPN1(io.request.vaddress) ## 0.U(2.W)
    io.sma.readType := MemoryAccessLength.word
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      when(!io.sma.error) {
        // printf("Walk vpn1: addr = %d, pte = %d\n", io.sma.address, io.sma.data)
        val pte = io.sma.data
        vpn1Reg := pte // Save
        when(pte(3, 1) === "b000".U) { // Next
          when(pte(0)) { // OK
            statusReg := Status.vpn0
          }.otherwise {
            alloc(pte, true.B, false.B)
            statusReg := Status.lookup // Return
          }
        }.otherwise { // Leaf
          alloc(pte, true.B, false.B)
          statusReg := Status.lookup // Return
        }
      }.otherwise { // Memory error
        io.request.error := MemoryErrorCode.memoryFault
        io.request.ready := true.B
        statusReg := Status.idle
      }
    }
  }

  // TLB walk: vpn0
  when(statusReg === Status.vpn0) {
    // PTE-2 entry address (34 -> 32)
    io.sma.address := vpn1Reg(31, 10) ## Sv32.getVPN0(io.request.vaddress) ## 0.U(2.W)
    io.sma.readType := MemoryAccessLength.word
    io.sma.valid := true.B
    when(io.sma.ready) { // Finished
      val pte = io.sma.data
      when(!io.sma.error) { // Leaf
        // printf ("Walk vpn0: addr = %d, pte = %d\n", io.sma.address, io.sma.data)
        alloc(pte, false.B, vpn1Reg(5))
        statusReg := Status.lookup // Return
      }.otherwise { // Memory error
        io.request.error := MemoryErrorCode.memoryFault
        io.request.ready := true.B
        statusReg := Status.idle
      }
    }
  }
}
