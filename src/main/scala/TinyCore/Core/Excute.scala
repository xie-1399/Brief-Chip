package TinyCore.Core

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.10
 * Excute stage -> excute all kinds of instruction
 * =======================================================
 */


class Excute extends PrefixComponent{
  import Defines._
  val io = new Bundle{
    val opcode = slave(Opcode())
    val int_e = in Bool() //interrupt happens
    val int_addr = in UInt(InstBusAddrWidth bits)

    /* write the regs */
    val reg_wdata_o = out Bits(RegWidth bits)
    val reg_we_o = out Bool()
    val reg_waddr_o = out UInt(RegNumLog2 bits)
  }

  val ALU = new Area{



  }

  val MulDiv = new Area{

  }

  val Memory = new Area{

  }


}
