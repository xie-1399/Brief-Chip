package TinyCore.Peripheral

import Common.SIMCFG
import Common.SpinalTools.PrefixComponent
import spinal.lib._
import Common.SimUntils._
import spinal.core._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim._

import scala.collection.mutable
import scala.math.pow
import scala.util.Random

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the simple ram can use the apb bus read and write it
 * =======================================================
 */
class Apb3Ram(addrWidth:Int = 32,dataWidth:Int = 32) extends PrefixComponent{
  import TinyCore.Core.Constant.Parameters._
  val io = new Bundle{
    val apb = slave (Apb3(addrWidth,dataWidth))
  }
  val depth = RamSize / dataWidth
  val addr = log2Up(depth)
  val ram = Mem(Bits(dataWidth bits), depth)
  val readIt = io.apb.PENABLE && !io.apb.PWRITE && io.apb.PSEL(0)
  val writeIt = io.apb.PENABLE && io.apb.PWRITE && io.apb.PSEL(0)
  val delay = RegNext(readIt).init(False)
  io.apb.PREADY := delay || writeIt
  io.apb.PSLVERROR.clear()
  io.apb.PRDATA := ram.readSync(io.apb.PADDR(addr - 1 downto 0), enable = readIt)
  ram.write(io.apb.PADDR(addr - 1 downto 0),io.apb.PWDATA,enable = writeIt)
}

object Apb3Ram extends App{
  /* 16 KIB ram */
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new Apb3Ram()
    dut.ram.simPublic()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val driver = Apb3Driver(dut.io.apb, dut.clockDomain)
      val queue = new mutable.Queue[BigInt]()
      for(idx <- 0 until 128 * 4){
        val data = Random.nextInt(1024 * 1024)
        queue.enqueue(data)
        driver.write(BigInt(idx),data)
        assert(driver.read(idx) == queue.dequeue())
      }
      simSuccess()
  }
}
