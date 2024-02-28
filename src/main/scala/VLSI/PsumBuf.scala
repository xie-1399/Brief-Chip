package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * the partial sum buffer
 * =======================================================
 */

import spinal.core._
import spinal.lib._

import Constant._

class PsumBuf extends Component {

  val io = new Bundle{
    val pe_data = in Vec(SInt(PE_DataWidth bits),4)
    val p_write_zero = in Bool()
    val p_valid_data = in Bool()
    val p_init = in Bool()
    val odd_cnt = in Bool()

    val fifo_out = master Flow SInt(PE_DataWidth bits)
  }

  /* using two fifo */

}
