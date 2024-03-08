package TinyCore.Sim

/*========================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.6
 * simulate the AxiCrossBar with some test
 *========================================================
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.misc._
import spinal.lib.bus.amba3.apb._

/* the Axi4 crossbar seems ready */

class Axi4CrossBarSim extends Component {

  def getConfig() = {
    val config = Axi4Config(
      addressWidth = 34,
      dataWidth = 32,
      idWidth = 4,
      useLock = false,
      useRegion = false,
      useCache = false,
      useProt = false,
      useQos = false)
    config
  }

  val io = new Bundle{
    val axi4 = slave (Axi4Shared(config = getConfig()))
    val timerInterrupt = out Bool()
    val softwareInterrupt = out Bool()
  }

  val apbBridge = Axi4SharedToApb3Bridge(
    addressWidth = 34,
    dataWidth = 32,
    idWidth = 4
  )
  val apbclint = Apb3Clint(hartCount = 1)
  val apbDecoder = Apb3Decoder(
    master = apbBridge.io.apb,
    slaves = List(
      apbclint.io.bus -> (0x10000000, 64 KiB), /* 0x00000 - 0x10000*/
    )
  )

  val axiCrossBar = Axi4CrossbarFactory()
  axiCrossBar.addSlaves(apbBridge.io.axi -> (0x10000000L,256 MiB))
  axiCrossBar.addConnections(io.axi4 -> List(apbBridge.io.axi))

  /* with the crossbar pipeline */
  axiCrossBar.addPipelining(apbBridge.io.axi)((read,bus)=>{
    read.readRsp << bus.readRsp
    read.writeRsp << bus.writeRsp
    read.writeData >> bus.writeData
    read.sharedCmd >> bus.sharedCmd
  })
  
  axiCrossBar.build()
  io.timerInterrupt := apbclint.io.timerInterrupt.asBool
  io.softwareInterrupt := apbclint.io.softwareInterrupt.asBool
}


object Axi4CrossBarSim extends App{

  SpinalSystemVerilog(new Axi4CrossBarSim())
}