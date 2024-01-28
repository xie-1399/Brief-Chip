package TinyCore.Peripheral

import Common.SpinalTools.PrefixComponent
import spinal.lib._
import spinal.core._
import scala.math.pow
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the simple ram can use the rib bus read and write it
 * =======================================================
 */


class ram(addrWidth:Int = 10,dataWidth:Int = 32) extends PrefixComponent{

  val io = new Bundle{
    val wr = in Bool()
    val addr = in UInt(addrWidth bits)
    val dataIn = in Bits(dataWidth bits)
    val dataOut = out Bits(dataWidth bits)
  }
  def depth = pow(2,addrWidth).toInt
  val ram = Mem(Bits(dataWidth bits),depth)

  io.dataOut := ram.readSync(io.addr,!io.wr)
  ram.write(io.addr,io.dataIn,enable = io.wr)
}

object ram extends App{
  SpinalSystemVerilog(new ram())
}
