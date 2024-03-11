package TinyCore.Core.Fetch

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant._
import Defines._
import Instruction._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * pc jump is simple enough that only control the inst
 * (Hold + Jump + Reset) Control
 * =======================================================
 */

class PC_Control extends PrefixComponent{

  val io = new Bundle{
    val jtagReset = in Bool()
    val jump = in Bool()
    val jumpAddr = in UInt(InstBusAddrWidth bits)
    val hold = in UInt(HoldWidth bits)
    val pcOut = out UInt(InstBusAddrWidth bits)
    val fetchValid = out Bool() /* show if the fetch bus cmd is valid */
  }

  val PC = Reg(UInt(InstBusAddrWidth bits)).init(CPUReset)
  val fetchValid = True
  when(io.jtagReset){
    PC := CPUReset
  }.elsewhen(io.jump === JumpEnable){
    PC := io.jumpAddr
  }.elsewhen(io.hold >= Hold_PC){
    PC := PC
    fetchValid.clear()
  }.otherwise{
    PC := PC + 4
  }
  io.pcOut := PC
  io.fetchValid := fetchValid
}