package TinyCore.Core

import Common.SpinalTools.PrefixComponent
import spinal.lib._
import spinal.core._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * pc jump is simple enough that only control the inst
 * (Hold + Jump + Reset) Control
 * =======================================================
 */

class PC_Control extends PrefixComponent{
  import Defines._

  val io = new Bundle{
    val jtagReset = in Bool()
    val jump = in Bool()
    val jumpAddr = in UInt(InstBusAddrWidth bits)
    val hold = in UInt(HoldWidth bits)
    val pcOut = out UInt(InstBusAddrWidth bits)
  }

  val PC = RegInit(UInt(InstBusAddrWidth bits)).init(CPUReset)

  when(io.jtagReset){
    PC := CPUReset
  }.elsewhen(io.jump === JumpEnable){
    PC := io.jumpAddr
  }.elsewhen(io.hold >= Hold_PC){
    PC := PC
  }.otherwise{
    PC := PC + 4
  }

  io.pcOut := PC
}
