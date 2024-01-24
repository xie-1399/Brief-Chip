package TinyCore.Utils

/**
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.24
 * using to build some DFF until Regs for the Pipeline(all latency 1)
 */

import spinal.core._
import SpinalTools.PrefixComponent

class Pipe_DFF(width:Int = 32) extends PrefixComponent {
  /* with the default value and hold signal */
  val io = new Bundle{
    val hold = in Bool()
    val default = in Bits(width bits)
    val din = in Bits(width bits)
    val dout = out Bits(width bits)
  }
  val dout = RegInit(io.default)
  when(io.hold){
    dout := io.default
  }.otherwise{
    dout := io.din
  }
  io.dout := dout
}

/**
 * @param init the reset value of output
 * @param width the input and out put width
 */


class Reset_init_DFF(init:Int = 0,width:Int = 32) extends PrefixComponent {
  /* when the reset is happen -> decide the output with init value */
  val io = new Bundle{
    val din = in Bits(width bits)
    val dout = out Bits(width bits)
  }
  val dout = Reg(Bits(width bits)).init(init)
  dout := io.din
  io.dout := dout
}

class Reset_default_DFF(width:Int = 32) extends PrefixComponent {
  /* when the reset is happen -> decide the output with init value */
  val io = new Bundle{
    val din = in Bits(width bits)
    val dout = out Bits(width bits)
    val default = in Bits(width bits)
  }
  val dout = RegInit(io.default)
  dout := io.din
  io.dout := dout
}

class Reset_en_DFF(init:Int = 0,width:Int = 32) extends PrefixComponent {
  /* when the reset is happen -> decide the output with init value */
  val io = new Bundle{
    val din = in Bits(width bits)
    val dout = out Bits(width bits)
    val enable = in Bool()
  }
  val dout = Reg(Bits(width bits)).init(init)
  when(io.enable){
    dout := io.din
  }
  io.dout := dout
}

object DFF{
  def main(args: Array[String]): Unit = {
    SpinalVerilog(new Reset_init_DFF())
    SpinalVerilog(new Pipe_DFF())
    SpinalVerilog(new Reset_default_DFF())
    SpinalVerilog(new Reset_en_DFF())
  }
}