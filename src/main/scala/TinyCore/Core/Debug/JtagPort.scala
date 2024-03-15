package TinyCore.Core.Debug

import spinal.core._
import TinyCore.Core.Constant._
import Defines._
import spinal.lib._

case class JtagPort() extends Bundle with IMasterSlave{
  val jtag_We = Bool()
  val jtag_Addr = UInt(RegNumLog2 bits)
  val jtag_Rdata = Bits(RegWidth bits)
  val jtag_Wdata = Bits(RegWidth bits)

  override def asMaster(): Unit = {
    out(jtag_We,jtag_Addr,jtag_Wdata)
    in(jtag_Rdata)
  }

  /* slave block jtag*/
  def init(): Unit = {
    this.jtag_Addr := 0
    this.jtag_Wdata := 0
    this.jtag_We := False
  }
}
