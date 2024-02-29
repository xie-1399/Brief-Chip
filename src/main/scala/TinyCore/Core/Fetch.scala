package TinyCore.Core

import Common.SpinalTools.PrefixComponent
import spinal.lib._
import spinal.core._
import TinyCore.Utils._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * Fetch stage (how to deal with the interrupt cmd)
 * =======================================================
 */

class Fetch extends PrefixComponent{
  import Defines._
  import Instruction._

  val io = new Bundle{

    val inst_i = in Bits(InstBusDataWidth bits) /* the instruction from the bus */
    val inst_addr_i = in UInt(InstBusAddrWidth bits)

    val hold = in UInt(HoldWidth bits)

    val inst_o = out Bits(InstBusDataWidth bits)
    val inst_addr_o = out UInt(InstBusAddrWidth bits)
  }
  /* hold the fetch stage or not */
  val hold_en = (io.hold >= Hold_Fetch)

  /* pass the pc and inst with the pipe going */
  val dff_inst = new Pipe_DFF(InstBusDataWidth)
  dff_inst.io.din := io.inst_i
  dff_inst.io.hold := hold_en
  dff_inst.io.default := INST_DEFAULT
  io.inst_o := dff_inst.io.dout

  val dff_addr = new Pipe_DFF(InstBusDataWidth)
  dff_addr.io.din := io.inst_addr_i.asBits
  dff_addr.io.hold := hold_en
  dff_addr.io.default := ZeroWord
  io.inst_addr_o := dff_addr.io.dout.asUInt
}
