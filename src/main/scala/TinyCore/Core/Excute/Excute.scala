package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import TinyCore.Core.Decode._
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant.Parameters._
import TinyCore.Core.Constant.Defines._
import TinyCore.Core.Decode._
import spinal.core.sim._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.10
 * Excute stage -> excute all kinds of instruction
 * =======================================================
 */

class Excute extends PrefixComponent{
  val io = new Bundle{
    val opcode = in (CtrlSignals(decodeConfig)) /* using the decode v2*/
    val ex_valid = in Bool()
    val inst_i = in Bits (InstBusDataWidth bits)
    val inst_addr_i = in UInt(InstBusAddrWidth bits)
    /* from the regfile */
    val regs = slave(regSignals())
    /* write the regs */
    val reg_wdata_o = out Bits(RegWidth bits)
    val reg_we_o = out Bool()
    val reg_waddr_o = out UInt(RegNumLog2 bits)
  }

  def isMulDiv(ctrl: CtrlSignals): Bool = {
    import ALU._
    val res = ctrl.alu.mux(
      MUL -> True,
      MULH -> True,
      MULHSU -> True,
      MULHU -> True,
      DIV -> True,
      DIVU -> True,
      REMU -> True,
      REM -> True,
      default -> False
    )
    res
  }

  val op1 = Bits(Xlen bits)
  op1 := io.opcode.op1.mux(
    OP1.RS1 -> io.regs.reg1_rdata_o,
    OP1.PC -> io.inst_addr_i.asBits,
    default -> B(0,Xlen bits)
  )
  val op2 = Bits(Xlen bits)
  op2 := io.opcode.op2.mux(
    OP2.RS2 -> io.regs.reg2_rdata_o,
    OP2.IMM_I -> Repeat(io.inst_i.msb,20) ## io.inst_i(31 downto 20),
    OP2.IMM_B -> Repeat(io.inst_i.msb,20) ## io.inst_i(7) ## io.inst_i(30 downto 25) ## io.inst_i(11 downto 8) ## B"0",
    OP2.IMM_U -> io.inst_i(31 downto 12) ## Repeat(B(0,1 bits),12),
    OP2.IMM_S -> Repeat(io.inst_i.msb,20) ## io.inst_i(31 downto 25) ## io.inst_i(11 downto 7),
    OP2.IMM_J -> Repeat(io.inst_i.msb, 12) ## io.inst_i(19 downto 12) ## io.inst_i(20) ## io.inst_i(30 downto 21) ## B"0",
    default -> B(0,Xlen bits)
  )

  val ismuldiv = isMulDiv(io.opcode)

  val alu = new Area{
    /* deal with the alu options */
    val aluPlugin = new ALUPlugin()
    aluPlugin.io.valid := io.opcode.illegal && io.ex_valid
    aluPlugin.io.alu := io.opcode.alu
    aluPlugin.io.op1 := op1
    aluPlugin.io.op2 := op2
  }

  val muldiv = new Area{
    val muldivPlugin = new SimpleMulDivPlugin()
    muldivPlugin.io.valid := io.opcode.illegal && io.ex_valid
    muldivPlugin.io.alu := io.opcode.alu
    muldivPlugin.io.op1 := op1
    muldivPlugin.io.op2 := op2
  }

  val Memory = new Area{

  }

  val Csr = new Area{

  }

  io.reg_we_o := io.regs.reg_we
  io.reg_waddr_o := io.regs.reg_waddr
  io.reg_wdata_o := Mux(ismuldiv,alu.aluPlugin.io.res,muldiv.muldivPlugin.io.res)

  val whiteBox = new Area{
    /* for the debug use */
    val lastStagePC = Reg(UInt(Xlen bits)).init(0)
    lastStagePC.simPublic()
    when(io.ex_valid && !io.opcode.illegal){
      lastStagePC := io.inst_addr_i
    }
  }
}

object Excute extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Excute())
}