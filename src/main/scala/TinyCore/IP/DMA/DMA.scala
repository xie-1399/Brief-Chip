package TinyCore.IP.DMA

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.4.2
 * set the dma top module
 * =======================================================
 */

import Common.SIMCFG
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver
import spinal.lib.bus.regif._

import scala.util.Random


case class DMAConfig(regWidth:Int = 32)

class DMA(c:DMAConfig) extends Component {
  import AccessType._
  import Package._
  val io = new Bundle{
    val apb = slave (Apb3(apbConfig))
    // val axi = master(Axi4(axi4Config))
  }

  val regfile = new Area{
    /* using the apb3 factory */
    val busif = Apb3BusInterface(io.apb,(0x0000, 100 Byte))
    val Go_Pulse = busif.newRegAt(0x0,doc = "Pulse")
    // val IRQEnable = busif.newRegAt(0x0,doc = "pluse")
    // val IRQClear_Pluse = busif.newRegAt(0x0,doc = "pluse")
    val SrcAddr = busif.newRegAt(0x10,doc = "SrcAddr")
    val DstAddr = busif.newRegAt(0x20,doc = "DstAddr")
    val Length = busif.newRegAt(0x30,doc = "Length")
    val Busy = busif.newRegAt(0x40,doc = "Busy")

    // set the reg data type
    val pluse = Go_Pulse.field(Bool(),RW)
    val busy = Busy.field(Bool(),RW)
    val length = Length.field(UInt(c.regWidth bits),RW)
    val dstAddr = DstAddr.field(UInt(c.regWidth bits),RW)
    val srcAddr = SrcAddr.field(UInt(c.regWidth bits),RW)
  }



  val reader = new Area{

  }


  val writer = new Area{

  }

  val controller = new Area{

  }

}

object DMA extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new DMA(DMAConfig())
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val driver = Apb3Driver(dut.io.apb,dut.clockDomain)
      /* check the DMA config */
      def configCheck() = {
        val addr = Array.fill(3)(Random.nextInt(1024 * 1024))
        val addrList = Array[BigInt](0x10,0x20,0x30)
        addr.zipWithIndex.foreach{
          f =>
            driver.write(addrList(f._2),f._1)
            assert(driver.read(addrList(f._2)) == f._1)
        }
        driver.write(0x0,3)
        assert(driver.read(0x0).toLong == 1)
      }
      configCheck()
      simSuccess()
  }

}
