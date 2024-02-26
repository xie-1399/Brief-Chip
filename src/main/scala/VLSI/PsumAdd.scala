package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * using the 3 stages adder tree to calculate the 4 pe sum
 * =======================================================
 */

import spinal.core._
import spinal.core._

class PsumAdd(dataWidth:Int = 25) extends Component {

  val io = new Bundle{
    val pe_data = in(Vec(SInt(dataWidth bits),4))
    val fifo_data = in SInt(dataWidth bits)
    val pe_psum = out(SInt(dataWidth bits))
  }

  /* 3 stages pipe adder-tree */
  val psum0 = Reg(SInt(dataWidth bits)).init(0)
  val psum1 = Reg(SInt(dataWidth bits)).init(0)
  val psum2 = Reg(SInt(dataWidth bits)).init(0)
  val TotalPsum = Reg(SInt(dataWidth bits)).init(0)

  psum0 := io.pe_data(0) + io.pe_data(1)
  psum1 := io.pe_data(2) + io.pe_data(3)
  psum2 := psum0 + psum1
  TotalPsum := io.fifo_data + psum2

  io.pe_psum := TotalPsum
}


object PsumAdd extends App{
  SpinalSystemVerilog(new PsumAdd(25))
}