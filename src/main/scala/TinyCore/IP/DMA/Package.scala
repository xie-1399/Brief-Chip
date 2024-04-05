package TinyCore.IP.DMA
import spinal.core._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi.Axi4Config

object Package {
  def MAX_BURST_BYTES  = 4096
  def MAX_BURST_LENGTH = 256

  def apbConfig = Apb3Config(32,32)
  // def axi4Config = Axi4Config(useQos = false,useCache = false,useProt = false)
}

class CmdBundle(AddrWidth: Int) extends Bundle {
  import Package._
  val NumBytes = UInt(log2Up(MAX_BURST_BYTES) bits)
  val Address = UInt(AddrWidth bits)
}

class TransBundle(AddrWidth: Int) extends Bundle {
  val NumBytes = UInt(32 bits)
  val Address = UInt(AddrWidth bits)
}