package TinyCore.Core.Excute

import spinal.core._
import spinal.lib._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.23
 * simple compare the two numbers
 * =======================================================
 */

class Compare(srcWidth:Int) extends Component{
  //compare two numbers
  val io = new Bundle{
    val sign = in Bool()
    val src0 = in Bits(srcWidth bits)
    val src1 = in Bits(srcWidth bits)
    val ltx = out Bool()
    val eq = out Bool()
  }

  //sign or unsign compare
  when(io.sign) {
    io.ltx := io.src0.asSInt < io.src1.asSInt
  }.otherwise {
    io.ltx := io.src0.asUInt < io.src1.asUInt
  }
  io.eq := io.src0 === io.src1
}
