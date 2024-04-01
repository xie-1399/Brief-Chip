package TinyCore

import Common.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import TinyCore.Sim.Axi4MemorySimV2
import spinal.core.sim._
import spinal.core._
import TinyCore.Core._
import SimTools.Tools._
import TinyCore.Soc.CPU
import org.scalatest.funsuite.AnyFunSuite

/* run all the benchmarks here */

class BenchMark extends AnyFunSuite{

  test("RV bench"){
    /* the rv test will be tested on the kernel */
    SIMCFG(gtkFirst = true).compile {
      val dut = new CPU()
      val core = dut.Soc.kernel.core
      core.regfile.regfile.simPublic()
      core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(5 us)
        /* first run the makefile get the arithmetic binary and hazard works also */
        dut.systemClockDomain.forkStimulus(10)
        val mem = Axi4MemorySimV2(dut.io.axi4, dut.systemClockDomain, SimConfig.axi4simConfig)
        val address = 0x80000000l
        val TestCase = SimConfig.testFile.size
        val files = SimConfig.testFile.keys.toArray
        val passSymbols = SimConfig.testFile.values.toArray
        var passNum = 0
        var start = true
        def startIt() = {
          mem.memory.loadBinary(address, files(passNum))
          println(s"the memory load ${files(passNum).split("/").last} finish!")
          mem.start()
          Axi4Init(dut.io.axi4)
          dut.io.jtagReset #= false
          dut.io.reset #= true
          dut.io.gpi #= 0
          dut.systemClockDomain.waitSampling()
          dut.io.reset #= false
          dut.systemClockDomain.waitSampling(3)
          start = false
        }
        startIt()
        val lastStagePC = dut.Soc.kernel.core.whiteBox.lastStagePC
        dut.systemClockDomain.onSamplings{
          dut.io.reset #= false
          if(lastStagePC.toLong.toHexString == passSymbols(passNum) && !start){
            println(s"${files(passNum).split("/").last} pass...\n")
            passNum += 1
            if (passNum == TestCase) {simSuccess()}
            mem.memory.loadBinary(address, files(passNum))
            println(s"the memory load ${files(passNum).split("/").last} finish!")
            dut.io.reset #= true
            Axi4Init(dut.io.axi4)
            dut.io.jtagReset #= false
            mem.reset()
          }
        }
      }
    }

  test("internal ram or rom"){
    

  }


  test("Dhrystone"){

  }


  test("Coremark"){

  }

  test("RTOS"){

  }


}
