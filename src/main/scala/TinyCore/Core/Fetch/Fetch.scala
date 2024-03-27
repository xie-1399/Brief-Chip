package TinyCore.Core.Fetch

import Common.SpinalTools.PrefixComponent
import TinyCore.Utils._
import spinal.core._
import spinal.core.sim._
import TinyCore.Core.Constant._
import TinyCore.Core.Pipeline._
import Defines._
import spinal.lib._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * Fetch stage (how to deal with the interrupt cmd)
 * =======================================================
 */

class Fetch extends PrefixComponent{

  val io = new Bundle{
    val fetchInPipe = slave(pipeSignals())
    val fetchOutPipe = master(pipeSignals())
    val hold = in UInt(HoldWidth bits)
  }
  /* hold the fetch stage or not */
  val hold_en = (io.hold >= Hold_Fetch)
  val holdInst = Reg(UInt(2 bits)).init(0)
  val remain = holdInst > 0

  when(io.fetchInPipe.valid && hold_en){
    holdInst := holdInst + 1
  }

  val getInst = (RegNext(io.fetchInPipe.valid) .init(False)).simPublic()
  when(!hold_en && remain) {
    holdInst := holdInst - 1
  }
  getInst.clearWhen(hold_en)
  getInst.setWhen(remain && !hold_en)
  io.fetchOutPipe.valid := getInst
  /* pass the pc and inst with the pipe going */
  val dff_pc = new Pipe_DFF(io.fetchInPipe.pc.getBitsWidth)
  dff_pc.io.din := io.fetchInPipe.pc.asBits
  dff_pc.io.hold := hold_en
  dff_pc.io.default := 0
  io.fetchOutPipe.pc := dff_pc.io.dout.asUInt

  val dff_Inst = new Pipe_DFF(io.fetchInPipe.inst.getBitsWidth)
  dff_Inst.io.din := io.fetchInPipe.inst
  dff_Inst.io.hold := hold_en
  dff_Inst.io.default := 0
  io.fetchOutPipe.inst := dff_Inst.io.dout
}
