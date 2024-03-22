package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant.Defines._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.9
 * control the interrupt and hold signals
 * =======================================================
 */

class Ctrl extends PrefixComponent{

  /* control the hold signals */
  /* show which stage error and send it into the csr running error check */

  val io = new Bundle{
    val stageError = in UInt(log2Up(stageNum) bits) /* exception capture */
    val stageId = in UInt(log2Up(stageNum) bits)

    /* hold signals from the excute */
    val hold_ex = in UInt(HoldWidth bits)
    val holdOut = out UInt(HoldWidth bits)
  }
  /* big hold signals */
  io.holdOut := io.hold_ex

}
