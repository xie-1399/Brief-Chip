package TinyCore.Peripheral
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the simple rom can use the rib bus read
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import scala.math.pow

class rom(addrWidth:Int = 10,dataWidth:Int = 32) extends PrefixComponent{
  val io = new Bundle {
    val addr = slave Flow(UInt (addrWidth bits))
    val dataOut = out Bits (dataWidth bits)
  }
  def depth = pow(2, addrWidth).toInt
  val rom = Mem(Bits(dataWidth bits), depth)
  io.dataOut := rom.readSync(io.addr.payload,enable = io.addr.valid)
}

object rom extends App{
  SpinalSystemVerilog(new rom())
}