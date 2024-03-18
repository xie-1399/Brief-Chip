package TinyCore.Peripheral
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the simple rom can use the apb3 bus read
 * =======================================================
 */

import Common.SIMCFG
import Common.SpinalTools.PrefixComponent
import Common.SimUntils._
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver

import scala.math.pow

class Apb3Rom(addrWidth:Int = 10,dataWidth:Int = 32) extends PrefixComponent{
  val io = new Bundle {
    val apb = slave (Apb3(addrWidth,dataWidth))
  }
  def depth = pow(2, addrWidth).toInt
  val rom = Mem(Bits(dataWidth bits), depth)
  // rom.initBigInt(Seq(0x00200313,0x00200313))
  val readIt = io.apb.PENABLE && !io.apb.PWRITE && io.apb.PSEL(0)
  val delay = RegNext(readIt).init(False)
  io.apb.PREADY := delay
  io.apb.PSLVERROR.clear()
  io.apb.PRDATA := rom.readSync(io.apb.PADDR,enable = readIt)
}

/* seems the memory sim init is ready*/
object Apb3Rom extends App{
  /* init the rom like this */
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new Apb3Rom()
    dut.rom.simPublic()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val driver = Apb3Driver(dut.io.apb,dut.clockDomain)
      val getValue = BinFile2BigInt("ext/codes/Arithmetic.bin",dut.rom.wordType.getBitsWidth,count = 1024)
      for(idx <- 0 until dut.rom.wordCount){
        setBigInt(dut.rom,idx.toLong,getValue(idx))
        assert(driver.read(idx).toLong.toHexString == getValue(idx).toLong.toHexString)
      }
      simSuccess()
  }

}