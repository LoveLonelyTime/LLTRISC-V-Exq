package bergamot.export

import chisel3._

import bergamot.core._
import bergamot.core.debug.DebugIO

import bergamot.peripheral.MemoryHole
import bergamot.peripheral.VirtualUART
import bergamot.peripheral.MachineTimer
import bergamot.peripheral.ROM
import bergamot.peripheral.VirtualRAM

import bergamot.interconnect.AXIInterconnect

/*
 * Verilator test core
 *
 * For verilator simulator
 *
 * Copyright (C) 2024-2025 LoveLonelyTime
 */

private class VerilatorTestCore extends Module {

  private val config = CoreConfig.default.copy(pcInit = "hffff0000")

  val io = IO(new Bundle {
    val rdAddress = Output(UInt(32.W))
    val rdData = Input(UInt(32.W))
    val wrAddress = Output(UInt(32.W))
    val wrData = Output(UInt(32.W))
    val wrStrobe = Output(UInt(4.W))

    val dataOut = Output(UInt(8.W))
    val send = Output(Bool())

    val debug = new DebugIO()
  })

  private val core = Module(new BergamotCore(config))

  private val interconnect = Module(
    new AXIInterconnect(
      Seq(
        "h00000000", // hole
        "h2000000", // mtime
        "h10000000", // uart
        "h80000000", // ram
        "hffff0000" // rom
      )
    )
  )
  private val hole = Module(new MemoryHole())
  private val uart = Module(new VirtualUART("h10000000"))
  private val mtimer = Module(new MachineTimer("h2000000"))
  private val rom = Module(new ROM(32, "hffff0000", "boot.hex"))
  private val ram = Module(new VirtualRAM("h80000000"))

  io.rdAddress := ram.io.rdAddress
  ram.io.rdData := io.rdData
  io.wrAddress := ram.io.wrAddress
  io.wrData := ram.io.wrData
  io.wrStrobe := ram.io.wrStrobe

  io.dataOut := uart.io.dataOut
  io.send := uart.io.send

  interconnect.io.slaves(0) <> hole.io.axi
  interconnect.io.slaves(1) <> mtimer.io.axi
  interconnect.io.slaves(2) <> uart.io.axi
  interconnect.io.slaves(3) <> ram.io.axi
  interconnect.io.slaves(4) <> rom.io.axi

  core.io.mtime := mtimer.io.mtime
  core.io.mtimeIRQ := mtimer.io.irq
  core.io.axi <> interconnect.io.master

  core.io.debug <> io.debug
}
