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
  test("Arithmetic") {
    SIMCFG().compile {
      val dut = new Kernel()
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        dut.systemClockDomain.forkStimulus(10)
        val mem = Axi4MemorySimV2(dut.io.axi4, dut.systemClockDomain, SimConfig.axi4simConfig)
        /* first run the makefile get the arithmetic binary */
        def passSymbol = "80000058"
        mem.memory.loadBinary(0x80000000l, "ext/codes/Arithmetic/Arithmetic.bin") // add the test file
        mem.start()
        val traces = readFile("src/test/scala/TinyCore/Trace/Arithmetic").toArray
        def init() = {
          Axi4Init(dut.io.axi4)
          dut.io.jtagReset #= false
          dut.systemClockDomain.waitSampling(5)
        }
        init()
        val lastStagePC = dut.core.whiteBox.lastStagePC
        var index = 0
        dut.systemClockDomain.onSamplings {
          if(dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 1){
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          if (lastStagePC.toLong.toHexString == passSymbol) {
            simSuccess()
          }
        }
    }
  }

  test("lsu test"){

    /* write the memory and read the memory with mask on*/



  }

}
