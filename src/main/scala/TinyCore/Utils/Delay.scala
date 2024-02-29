package TinyCore.Utils

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.24
 * delay the input with config cycles out
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._


class Delay_Hand(cycles:Int = 2,width:Int = 32) extends PrefixComponent{
  val io = new Bundle {
    val din = in Bits (width bits)
    val dout = out Bits (width bits)
  }

  val sync_dat = Vec(Bits(width bits),cycles)

  for(idx <- 0 until cycles){
    if(idx == 0){
      val dff = new Reset_init_DFF(init = 0, width = width)
      dff.io.din := io.din
      sync_dat(idx) := dff.io.dout
    }
    else{
      val dff = new Reset_init_DFF(init = 0, width = width)
      dff.io.din := sync_dat(idx)
      sync_dat(idx) := dff.io.dout
    }

  }
  io.dout := sync_dat(cycles - 1)
}


/**
 * Delay with the spinal lib Delay
 * using lots of regs
 */

class Delay_Lib(cycles:Int = 2,width:Int = 32) extends PrefixComponent{
  val io = new Bundle{
    val din = in Bits(width bits)
    val dout = out Bits(width bits)
  }
  /* so easy to implement the delay with cycles */
  io.dout := Delay(io.din,cycles,init = B(0,width bits))
}


object DelayIt{
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Delay_Lib(4))
    SpinalVerilog(new Delay_Hand(4))
  }
}


object Delay_testIt extends App{

}