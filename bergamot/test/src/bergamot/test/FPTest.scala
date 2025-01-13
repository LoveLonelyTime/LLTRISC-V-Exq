package bergamot.test

import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chiseltest._
import bergamot.core.fpu.FPUnbox
import bergamot.core.fpu.IEEESpec32
import bergamot.core.fpu.FPBox
import bergamot.core.fpu.FPRoundoff
import bergamot.core.fpu.FPDiv
import bergamot.core.fpu.FPSqrt
import bergamot.core.fpu.FPEntry
import bergamot.core.fpu.FPAddEntry
import bergamot.core.fpu.FPMul
import bergamot.core.fpu.FPMulEntry
import bergamot.core.fpu.FPAdd
import bergamot.core.fpu.FP
import bergamot.core.fpu.FPConv2FP

class FPMulAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(new FPEntry())
    val in2 = Input(new FPEntry())
    val in3 = Input(new FPEntry())
    val out = Output(new FPAddEntry())
  })

  private val fpMul = Module(new FPMul())
  fpMul.io.in1 := io.in1
  fpMul.io.in2 := io.in2

  private val addend2 = Wire(new FPMulEntry())
  FP.extend(io.in3, addend2)

  private val fpAdd = Module(new FPAdd())
  fpAdd.io.in1 := fpMul.io.out
  fpAdd.io.in2 := addend2

  io.out := fpAdd.io.out
}

class TestFPMulAdd extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val in3 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  private val fpMulAdd = Module(new FPMulAdd())
  private val fpUnbox1 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox2 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox3 = Module(new FPUnbox(IEEESpec32))
  private val fpBox = Module(new FPBox(IEEESpec32))
  fpUnbox1.io.in := io.in1
  fpUnbox2.io.in := io.in2
  fpUnbox3.io.in := io.in3

  fpMulAdd.io.in1 := fpUnbox1.io.out
  fpMulAdd.io.in2 := fpUnbox2.io.out
  fpMulAdd.io.in3 := fpUnbox3.io.out

  fpBox.io.in := fpMulAdd.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class TestFPDiv extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  private val fpDiv = Module(new FPDiv())
  private val fpUnbox1 = Module(new FPUnbox(IEEESpec32))
  private val fpUnbox2 = Module(new FPUnbox(IEEESpec32))
  private val fpBox = Module(new FPBox(IEEESpec32))
  fpUnbox1.io.in := io.in1
  fpUnbox2.io.in := io.in2

  fpDiv.io.in1 := fpUnbox1.io.out
  fpDiv.io.in2 := fpUnbox2.io.out

  fpBox.io.in := fpDiv.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class TestFPSqrt extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  private val fpSqrt = Module(new FPSqrt())
  private val fpUnbox = Module(new FPUnbox(IEEESpec32))
  private val fpBox = Module(new FPBox(IEEESpec32))
  fpUnbox.io.in := io.in

  fpSqrt.io.in := fpUnbox.io.out

  fpBox.io.in := fpSqrt.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class TestFPConv2FP extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  private val fpConv = Module(new FPConv2FP(32, true))
  fpConv.io.in := io.in

  private val fpBox = Module(new FPBox(IEEESpec32))
  fpBox.io.in := fpConv.io.out
  fpBox.io.roundoff := FPRoundoff.RNE
  io.out := fpBox.io.out
}

class FPTest extends AnyFlatSpec with ChiselScalatestTester {
  "FPTest" should "pass" in {
    test(new TestFPConv2FP()) { dut =>
      dut.io.in.poke("hFFFFFFF1".U)
      dut.clock.step()
      println(dut.io.out.peekInt())
    }
  }
}
