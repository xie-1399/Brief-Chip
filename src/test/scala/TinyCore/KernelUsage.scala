package TinyCore

import Common.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import TinyCore.Sim.Axi4MemorySimV2
import spinal.core.sim._
import spinal.core._
import TinyCore.Core._
import SimTools.Tools._


class KernelUsage extends AnyFunSuite {
  /* the trace is worked */
  def PASS(PC:String,passSymbol:String) = {
    if(PC == passSymbol) simSuccess()
  }

  def KernelInit(dut:Kernel,binary:String,address:Long = 0x80000000l) = {
    dut.systemClockDomain.forkStimulus(10)
    val mem = Axi4MemorySimV2(dut.io.axi4, dut.systemClockDomain, SimConfig.axi4simConfig)
    println("the memory load finish!")
    mem.memory.loadBinary(address, binary) // add the test file
    mem.start()
    Axi4Init(dut.io.axi4)
    dut.io.jtagReset #= false
    dut.systemClockDomain.waitSampling(5)
  }

  test("Arithmetic") {
    SIMCFG().compile {
      val dut = new Kernel()
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        /* first run the makefile get the arithmetic binary */
        KernelInit(dut,"ext/codes/Arithmetic/Arithmetic.bin")
        val traces = readFile("src/test/scala/TinyCore/Trace/Arithmetic").toArray
        def passSymbol = "80000068"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        var index = 0
        dut.systemClockDomain.onSamplings {
          if(dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 1){
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString,passSymbol)
        }
    }
  }

  /* wrong with the memory fetch happensInst happens */
  test("lsu test"){
    /* write the memory and read the memory with mask on*/
    SIMCFG().compile {
      val dut = new Kernel()
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        KernelInit(dut,"ext/codes/Memory/Memory.bin")
        def passSymbol = "800000e0"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        var index = 0
        dut.systemClockDomain.onSamplings {
          if (dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 1) {
            // assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString,passSymbol)
        }
    }
  }

  test("csr") {

  }

  test("jump test"){

  }

  test("exception and interrupt"){

  }

  test("fence and ecall"){

  }

  test("ebreak"){

  }

}
