package VLSI

import spinal.core._
import spinal.lib._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.27
 * the feature map buffer
 * =======================================================
 */

class WgtBuf extends Component {
  /* using 4 size vec as a simple buffer */
  import Constant._

  val io = new Bundle{
    val wgt_input = in SInt(Wgt_DataWidth bits)
    val wgt_read = in Bool()
    val wgt_bufout = out Vec(SInt(Wgt_DataWidth bits),BufSize)
  }

  val wgt_buf = Vec(Reg(SInt(Wgt_DataWidth bits)).init(0),BufSize)

  when(io.wgt_read){
    for(idx <- BufSize - 1 to 1 by -1){
      wgt_buf(idx) := wgt_buf(idx - 1)
    }
    wgt_buf(0) := io.wgt_input
  }

  io.wgt_bufout := wgt_buf
}
