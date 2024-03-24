package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant.Defines._
import spinal.lib._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.9
 * control the interrupt and hold signals
 * =======================================================
 */

case class JumpOp() extends Bundle with IMasterSlave{
  val jump = Bool()
  val jumpAddr = UInt(InstBusAddrWidth bits)
  override def asMaster(): Unit = {
    out(jump,jumpAddr)
  }
}

class Ctrl extends PrefixComponent{

  /* control the hold signals */
  /* show which stage error and send it into the csr running error check */

  val io = new Bundle {
    val fetchError = in Bool()
    val decodeError = in Bool()
    val excuteError = in Bool()
    val hold_ex = in UInt(HoldWidth bits)
    val holdOut = out UInt(HoldWidth bits)
    val jumpOp = slave(JumpOp())

    val Exception = out Bool()
    val flush = out Bool()
  }

    /* hold signals from the excute */

    val stageError = Reg(UInt(log2Up(stageNum) bits)).init(0)
    when(io.fetchError){
      stageError := 1
    }.elsewhen(io.decodeError){
      stageError := 2
    }.elsewhen(io.excuteError){
      stageError := 3
    }.otherwise{
      stageError := 0
    }
  io.holdOut := io.hold_ex
  io.Exception := io.excuteError || io.decodeError || io.fetchError
  io.flush := io.jumpOp.jump
}
