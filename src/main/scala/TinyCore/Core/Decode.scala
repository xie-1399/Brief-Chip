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

case class Opcode() extends Bundle {

}

class Decode() extends PrefixComponent{
  import Defines._
  import Instruction._

  val reg_addrWidth = log2Up(RegNum)
  val io = new Bundle{
    val inst_i = in Bits(InstBusDataWidth bits)
    val inst_addr_i = in UInt(InstBusAddrWidth bits)

    /* from the reg file */
    val reg1_rdata_i = in Bits(RegWidth bits)
    val reg2_rdata_i = in Bits(RegWidth bits)

    /* from the csr reg */
    val csr_rdata_i = in Bits(RegWidth bits)

    /* ex jump flag */
    val ex_jump_flag_i = in Bool()

    /* to regs */
    val reg1_raddr_o = out UInt(reg_addrWidth bits)
    val reg2_raddr_o = out UInt(reg_addrWidth bits)
    val csr_raddr_o = out UInt(MemAddrBus bits)

  }

}
