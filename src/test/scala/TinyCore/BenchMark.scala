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
import spinal.lib.bus.amba3.apb.Apb3

/* run all the benchmarks here */

class BenchMark extends AnyFunSuite{

  def catchW(apb: Apb3, base:BigInt, log: Boolean = true) = {
    val Cond = apb.PENABLE.toBoolean && apb.PREADY.toBoolean &&
      apb.PWRITE.toBoolean && apb.PSEL.toBigInt == 1 & apb.PADDR.toBigInt == base
    if (Cond) {
      if (log) print(apb.PWDATA.toBigInt.toChar) /* with char out */
    }
  }

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
          dut.io.uart.rxd #= true
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

  test("C Ram goes check") {
    /* test the inter ram */
    SIMCFG(gtkFirst = true).compile {
      val dut = new CPU()
      val core = dut.Soc.kernel.core
      core.regfile.regfile.simPublic()
      core.regfile.io.simPublic()
      dut.Soc.apb3Uart.io.apb.simPublic()
      dut
    }.doSimUntilVoid {
      dut => {
        SimTimeout(10 us)
        CPUInit(dut, "ext/C/Dhrystone/build/dhrystone.bin")
      }
    }
  }


  test("Dhrystone"){
    SIMCFG(gtkFirst = true,compress = true).compile {
      val dut = new CPU()
      val core = dut.Soc.kernel.core
      core.regfile.regfile.simPublic()
      core.regfile.io.simPublic()
      dut.Soc.apb3Uart.io.apb.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>{
        SimTimeout(200 us)
        CPUInit(dut, "ext/C/Dhrystone/build/dhrystone.bin")
        def passSymbol = "80000070"
        def failSymbol = "80000078"
        val lastStagePC = dut.Soc.kernel.core.whiteBox.lastStagePC
        dut.systemClockDomain.onSamplings {
          catchW(dut.Soc.apb3Uart.io.apb,0)
          PASS(lastStagePC.toLong.toHexString, passSymbol,failSymbol)
        }
      }
    }
  }


  test("Coremark"){

  }

  test("RTOS"){

  }


}
