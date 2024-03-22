package TinyCore.Core.Fetch

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant._
import Defines._
import spinal.lib._
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
    val pcOut = master Stream(out UInt(InstBusAddrWidth bits))
  }

  val PC = Reg(UInt(InstBusAddrWidth bits)).init(CPUReset)
  val resetDone = RegInit(False)
  resetDone := True

  val fetchValid = False
  when(resetDone){fetchValid.set()}

  when(io.jtagReset){
    PC := CPUReset
  }.elsewhen(io.jump){
    PC := io.jumpAddr
  }.elsewhen(io.hold >= Hold_PC){
    PC := PC
    fetchValid.clear()
  }.elsewhen(resetDone && io.pcOut.fire) {
    PC := PC + 4
  }.otherwise{
    PC := PC
  }
  io.pcOut.payload := PC
  io.pcOut.valid := fetchValid

}