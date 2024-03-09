package TinyCore.Core

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.29
 * Decode stage (with the comb logic )
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.lib._
import spinal.core._
import Instruction._
import Defines._

/* all as output signals */
case class Opcode(Xlen:Int = 32) extends Bundle with IMasterSlave {
  val op1 = Bits(Xlen bits)
  val op2 = Bits(Xlen bits)
  val op1_jump = Bits(Xlen bits)
  val op2_jump = Bits(Xlen bits)
  val op_inst = Bits(InstBusDataWidth bits)
  val op_pc = UInt(InstBusAddrWidth bits)
  val op_reg1_rdata = Bits(RegWidth bits)
  val op_reg2_rdata = Bits(RegWidth bits)
  val op_reg_we = Bool()
  val op_reg_waddr = UInt(RegNumLog2 bits)
  val op_csr_rdata = Bits(RegWidth bits)
  val op_csr_we = Bool()
  val op_csr_waddr = UInt(MemAddrBus bits)
  override def asMaster(): Unit = {
    out(op1,op2,op1_jump,op2_jump,op_inst,op_pc,
      op_reg1_rdata,op_reg2_rdata,op_reg_we,op_reg_waddr,
      op_csr_we,op_csr_waddr,op_csr_rdata)
  }
}

class Decode() extends PrefixComponent{
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

    /* decode result */
    val opcode = out(Opcode())
  }

  val opcode = io.inst_i(6 downto 0)
  val funct_3 = io.inst_i(14 downto 12)
  val funct_7 = io.inst_i(31 downto 25)
  val rd = io.inst_i(11 downto 7)
  val rs1 = io.inst_i(19 downto 15)
  val rs2 = io.inst_i(24 downto 20)

  switch(opcode){
    is(INST_TYPE_I){
      switch(funct_3){
        is(INST_ADDI, INST_SLTI, INST_SLTIU, INST_XORI, INST_ORI, INST_ANDI, INST_SLLI, INST_SRI){

        }
      }
    }
    // is()
    // is()
    // is()


  }

}
