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
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel(true)
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        /* first run the makefile get the arithmetic binary and hazard works also */
        KernelInit(dut,"ext/codes/Arithmetic/Arithmetic.bin")
        val traces = readFile("src/test/scala/TinyCore/Trace/Arithmetic").toArray
        def passSymbol = "80000088"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        var index = 0
        dut.clockDomain.onSamplings {
          if(dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 1){
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString,passSymbol)
        }
    }
  }

  test("lsu test"){
    /* write the memory and read the memory with mask on*/
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel(true)
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(100 ns)
        KernelInit(dut,"ext/codes/Memory/Memory.bin")
        def passSymbol = "80000138"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        val traces = readFile("src/test/scala/TinyCore/Trace/Memory").toArray
        var index = 0
        dut.clockDomain.onSamplings {
          dut.io.apb.PREADY #= true
          if (dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 4) {
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString,passSymbol)
        }
    }
  }

  test("jump test"){
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel(true)
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 ns)
        KernelInit(dut, "ext/codes/Jump/Jump.bin")
        def passSymbol = "80000090"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        val traces = readFile("src/test/scala/TinyCore/Trace/Jump").toArray
        var index = 0
        dut.clockDomain.onSamplings {
          dut.io.apb.PREADY #= true
          if (dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 4) {
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString, passSymbol)
        }
    }
  }

  test("csr") {
    /* test about the interrupt and exception also ecall and ebreak */
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel(true)
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(10 ns)
        /* Todo test about the exception later */
        KernelInit(dut, "ext/codes/Csr/Csr.bin")
        def passSymbol = "80000034"
        val lastStagePC = dut.core.whiteBox.lastStagePC
        val traces = readFile("src/test/scala/TinyCore/Trace/Csr").toArray
        var index = 0
        dut.clockDomain.onSamplings {
          dut.io.apb.PREADY #= false
          if (dut.core.regfile.io.write.we.toBoolean && dut.core.regfile.io.write.waddr.toBigInt == 4) {
            assert(traces(index) == dut.core.regfile.io.write.wdata.toBigInt.toString())
            index += 1
          }
          PASS(lastStagePC.toLong.toHexString, passSymbol)
        }
    }
  }

  /* compile the rv test bench first */
  test("RV bench example ") {
    SIMCFG(gtkFirst = true).compile {
      val dut = new Kernel(true)
      dut.core.regfile.regfile.simPublic()
      dut.core.regfile.io.simPublic()
      dut
    }.doSimUntilVoid {
      dut =>
        SimTimeout(100 ns)
        KernelInit(dut, "ext/bench/rv32ui-p-sb.bin")
        def passSymbol = "800004c4"
        /* pass symbol */
        val lastStagePC = dut.core.whiteBox.lastStagePC
        dut.clockDomain.onSamplings {
          PASS(lastStagePC.toLong.toHexString, passSymbol)
        }
    }
  }

}
