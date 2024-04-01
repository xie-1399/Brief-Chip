package SimTools

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._

import scala.io.Source
import TinyCore.Core._
import TinyCore.Sim.Axi4MemorySimV2
import TinyCore.SimConfig
import TinyCore.Soc.CPU
object Tools {

  def HexStringWithWidth(hex: String, width: Int, fill: String = "0", left: Boolean = true): String = {
    if (hex.length < width) {
      if (left) {
        fill * (width - hex.length) + hex
      } else {
        hex + fill * (width - hex.length)
      }
    } else {
      hex
    }
  }

  def readFile(filePath: String, logIt: Boolean = false) = {
    /* read the file line by line */
    val fileSource = Source.fromFile(filePath)
    if (logIt) {
      for (lines <- fileSource.getLines()) {
        println(lines) /* the file will show in the format */
      }
    }
    fileSource.getLines()
  }

  /* master init */
  def Axi4ReadOnlyInit(bus: Axi4ReadOnly, readOnly: Boolean) = {
    require(readOnly)
    val ar = bus.ar
    val r = bus.r
    ar.ready #= false
    r.valid #= false
    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0
  }

  /* bus init with  */
  def Axi4Init(bus: Axi4) = {
    val axi = bus
    val ar = axi.ar
    val r = axi.r
    val aw = axi.aw
    val w = axi.w
    val b = axi.b
    ar.ready #= false
    aw.ready #= false
    w.ready #= false
    r.valid #= false

    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0

    b.valid #= false
    if (b.config.useId) b.id #= 0
    if (b.config.useResp) b.resp #= 0
    if (b.config.useBUser) b.user #= 0
  }

  def PASS(PC: String, passSymbol: String,failSymbol:String = "-1") = {
    if (PC == passSymbol) simSuccess()
    if (PC == failSymbol) simFailure()
  }

  def KernelInit(dut: Kernel, binary: String, address: Long = 0x80000000l) = {
    dut.clockDomain.forkStimulus(10)
    val mem = Axi4MemorySimV2(dut.io.axi4, dut.clockDomain, SimConfig.axi4simConfig)
    println("the memory load finish!")
    mem.memory.loadBinary(address, binary) // add the test file
    mem.start()
    Axi4Init(dut.io.axi4)
    dut.io.jtagReset #= false
  }

  def CPUInit(dut: CPU, binary: String, address: Long = 0x80000000l) = {
    dut.systemClockDomain.forkStimulus(10)
    val mem = Axi4MemorySimV2(dut.io.axi4, dut.systemClockDomain, SimConfig.axi4simConfig)
    println("the memory load finish!")
    mem.memory.loadBinary(address, binary) // add the test file
    mem.start()
    Axi4Init(dut.io.axi4)
    dut.io.uart.rxd #= true
    dut.io.jtagReset #= false
    dut.io.reset #= true
    dut.systemClockDomain.waitSampling()
    dut.io.reset #= false
    dut.systemClockDomain.waitSampling(3)
  }

}
