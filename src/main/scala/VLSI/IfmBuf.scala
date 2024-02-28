package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.27
 * the feature map buffer
 * =======================================================
 */

import Common.SIMCFG
import spinal.core._
import spinal.lib._

class IfmBuf extends Component {
  /* using 4 size vec as a simple buffer */
  import Constant._

  val io = new Bundle{
    val ifm_input = in SInt(Ifm_DataWidth bits)
    val ifm_read = in Bool()
    val ifm_bufout = out Vec(SInt(Ifm_DataWidth bits),BufSize)
  }

  val ifm_buf = Vec(Reg(SInt(Ifm_DataWidth bits)).init(0),BufSize)

  when(io.ifm_read){
    for(idx <- BufSize - 1 to 1 by -1){
      ifm_buf(idx) := ifm_buf(idx - 1)
    }
    ifm_buf(0) := io.ifm_input
  }

  io.ifm_bufout := ifm_buf
}