package TinyCore

import Common.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import TinyCore.Core.Fetch._
import TinyCore.Sim.AxiReadonlyMemorysim
import spinal.core.sim._
import TinyCore.Core._
import spinal.lib.bus.amba4.axi.Axi4ReadOnly
import sys.process._
import scala.collection.mutable.Queue
import scala.util.Random
import SimTools.untils._


class KernelUsage extends AnyFunSuite {
  /* the trace is worked */
  test("Arithmetic") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel()
      dut.core.regfile.regfile.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        dut.systemClockDomain.forkStimulus(10)
        val mem = AxiReadonlyMemorysim(dut.io.axi4, dut.systemClockDomain, SimConfig.axi4ReadonlysimConfig)
        /* first run the makefile get the arithmetic binary */
        def passSymbol = "80000058"
//        val path = "pwd".!!
//        val cmd = s"cd ${path}/ext/codes"
//        try{
//          val res = cmd.!!
//          println(res)
//        }catch {
//          case e:Throwable => println("compile the arithmetic codes fail")
//        }

        mem.memory.loadBinary(0x80000000l, "ext/codes/Arithmetic.bin") // add the test file
        mem.start()

        def init() = {
          Axi4ReadOnlyInit(dut.io.axi4, true)
          dut.io.jtagReset #= false
          dut.systemClockDomain.waitSampling(5)
        }

        init()
        val lastStagePC = dut.core.whiteBox.lastStagePC
        dut.systemClockDomain.onSamplings {
          // println("x1 regs: " + dut.core.regfile.regfile.getBigInt(1).toLong)
          if (lastStagePC.toLong.toHexString == passSymbol) {
            simSuccess()
          }
        }
    }
  }
}
