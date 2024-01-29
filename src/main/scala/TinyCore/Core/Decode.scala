package TinyCore.Core

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.29
 * Fetch stage (how to deal with the interrupt cmd)
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.lib._
import spinal.core._


case class Opcode() extends Bundle{

}

class Decode() extends PrefixComponent{
  import Defines._
  import Instruction._

  val io = new Bundle{
    val inst_i = in Bits(InstBusDataWidth bits)
    val inst_addr_i = in UInt(InstBusAddrWidth bits)


  }

}
