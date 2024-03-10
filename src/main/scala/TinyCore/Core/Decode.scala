package TinyCore.Core

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.9
 * Decode stage (with the comb logic) simple version
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
  val io = new Bundle {
    val inst_i = in Bits (InstBusDataWidth bits)
    val inst_addr_i = in UInt (InstBusAddrWidth bits)
    /* from the reg file */
    val reg1_rdata_i = in Bits (RegWidth bits)
    val reg2_rdata_i = in Bits (RegWidth bits)
    /* from the csr reg */
    val csr_rdata_i = in Bits (RegWidth bits)
    /* ex jump flag */
    val ex_jump_flag_i = in Bool()
    /* to regs */
    val reg1_raddr_o = out UInt (reg_addrWidth bits)
    val reg2_raddr_o = out UInt (reg_addrWidth bits)
    val csr_raddr_o = out UInt (MemAddrBus bits) /* csr regs here */
    /* decode result */
    val opcode = master(Opcode())
    val error = out Bool()
    val decode_valid = in Bool() /* get the decode signal */
  }

  /* if using as the function like this */
  def getIMM(inst: Bits, immType: Bits): Bits = {
    val immBits = Bits(Xlen bits)
    immBits := immType.mux(
      B"000" -> inst(31 downto 20).resized,
      B"001" -> (inst(31 downto 25) ## inst(11 downto 7)).resized,
      B"010" -> (inst(31) ## inst(7) ## inst(30 downto 25) ## inst(11 downto 8)).resized,
      B"011" -> (inst(31 downto 12)).resize(Xlen),
      B"100" -> (inst(31) ## inst(19 downto 12) ## inst(20) ## inst(30 downto 21)).resize(Xlen),
      default -> B"0".resized
    )
    immBits
  }

  def signalExtend(raw:Bits,length:Int):Bits = {
    require(raw.getWidth <= length)
    val fills = length - raw.getWidth
    Repeat(raw.msb,fills).asBits ## raw
  }

  val opcode = io.inst_i(6 downto 0)
  val funct_3 = io.inst_i(14 downto 12)
  val funct_7 = io.inst_i(31 downto 25)
  val rd = io.inst_i(11 downto 7)
  val rs1 = io.inst_i(19 downto 15)
  val rs2 = io.inst_i(24 downto 20)
  val csrValue = io.inst_i(31 downto 20).resize(Xlen)

  val inst_op = Opcode()
  /* init value */
  inst_op.op_inst := io.inst_i /* the decode inst */
  inst_op.op_pc := io.inst_addr_i /* the decode pc */
  inst_op.op_reg1_rdata := io.reg1_rdata_i /* read the rs1 data */
  inst_op.op_reg2_rdata := io.reg2_rdata_i /* read the rs2 data */
  inst_op.op_csr_rdata := io.csr_rdata_i /* read the csr data */
  /* change with the instruction goes */
  inst_op.op_csr_we := False /* write csr signal */
  inst_op.op_csr_waddr := csrValue.asUInt /* csr write address */
  inst_op.op_reg_waddr := 0 /* write reg address */
  inst_op.op_reg_we := False /* write the reg signal */
  inst_op.op1 := 0
  inst_op.op2 := 0
  inst_op.op1_jump := 0 /* op1 jump address */
  inst_op.op2_jump := 0 /* op2 jum address */

  /* to regs */
  io.reg1_raddr_o := 0
  io.reg2_raddr_o := 0
  io.csr_raddr_o := csrValue.asUInt

  def Connect[T <: Data](signals:List[T],values:List[T]): Unit = {
    require(signals.length == values.length)
    signals.zipWithIndex.foreach(signal => signal._1 := values(signal._2))
  }
  val error = RegInit(False)

  when(io.decode_valid){
    switch(opcode) {
      is(INST_TYPE_I) {
        switch(funct_3) {
          is(INST_ADDI, INST_SLTI, INST_SLTIU, INST_XORI, INST_ORI, INST_ANDI, INST_SLLI, INST_SRI) {
            /* using the connect function compress the code  */
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2),
              List(True, rd.asUInt, rs1.asUInt, U(0, log2Up(RegNum) bits), io.reg1_rdata_i, signalExtend(io.inst_i(31 downto 20), Xlen))) /* imm with the signal extends */
          }
        }
      }
      is(INST_TYPE_R_M){
        when(funct_7 === 0 || funct_7 === 32){
          switch(funct_3){
            is(INST_ADD_SUB, INST_SLL, INST_SLT, INST_SLTU, INST_XOR, INST_SR, INST_OR, INST_AND){
              Connect(List(inst_op.op_reg_we,inst_op.op_reg_waddr,io.reg1_raddr_o, io.reg2_raddr_o,inst_op.op1, inst_op.op2),
                List(True,rd.asUInt, rs1.asUInt, rs2.asUInt,io.reg1_rdata_i,io.reg2_rdata_i))
            }
          }
        }.elsewhen(funct_7 === 1){
          switch(funct_3){
            /* mul and div */
            is(INST_MUL, INST_MULHU, INST_MULH, INST_MULHSU,INST_DIV, INST_DIVU, INST_REM, INST_REMU){
              Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2),
                List(True, rd.asUInt, rs1.asUInt, rs2.asUInt, io.reg1_rdata_i, io.reg2_rdata_i))
            }
          }
        }.otherwise{
          error.set()
        }
      }
      is(INST_TYPE_L){
        switch(funct_3){
          is(INST_LB, INST_LH, INST_LW, INST_LBU, INST_LHU){
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2),
              List(True,rd.asUInt,rs1.asUInt,U(0, log2Up(RegNum) bits),io.reg1_rdata_i,signalExtend(io.inst_i(31 downto 20), Xlen)))
          }
          default{
            error.set()
          }
        }
      }
      is(INST_TYPE_S){
        switch(funct_3){
          is(INST_SB, INST_SW, INST_SH){
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2),
              List(False,U(0, log2Up(RegNum) bits),rs1.asUInt, rs2.asUInt,io.reg1_rdata_i,Repeat(io.inst_i.msb,20) ## io.inst_i(31 downto 25) ## io.inst_i(11 downto 7)))
          }
          default{
            error.set()
          }
        }
      }
      is(INST_TYPE_B){
        switch(funct_3){ /* PC + imm*/
          is(INST_BEQ, INST_BNE, INST_BLT, INST_BGE, INST_BLTU, INST_BGEU){
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2,inst_op.op1_jump,inst_op.op2_jump),
              List(False,U(0, log2Up(RegNum) bits),rs1.asUInt, rs2.asUInt,io.reg1_rdata_i, io.reg2_rdata_i,io.inst_addr_i.asBits,Repeat(io.inst_i.msb,20) ## io.inst_i(7) ## io.inst_i(30 downto 25) ## io.inst_i(11 downto 8) ## B"0"))
          }
          default{
            error.set()
          }
        }
      }
      is(INST_JAL){ // PC = PC + imm / rd = pc + 4
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2, inst_op.op1_jump, inst_op.op2_jump),
          List(True, rd.asUInt, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), io.inst_addr_i.asBits, B(4,Xlen bits), io.inst_addr_i.asBits, Repeat(io.inst_i.msb, 12) ## io.inst_i(19 downto 12) ## io.inst_i(20) ## io.inst_i(30 downto 21) ## B"0"))
      }
      is(INST_JALR){ // PC = rs1 + imm
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2, inst_op.op1_jump, inst_op.op2_jump),
          List(True,rd.asUInt,rs1.asUInt,U(0, log2Up(RegNum) bits),io.inst_addr_i.asBits, B(4,Xlen bits),inst_op.op_reg1_rdata,Repeat(io.inst_i.msb,20) ## io.inst_i(31 downto 20)))
      }
      is(INST_LUI){
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o,inst_op.op1, inst_op.op2),
          List(True,rd.asUInt, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits),io.inst_i(31 downto 12) ## Repeat(B(0,1 bits),12),B(0,Xlen bits)))
      }
      is(INST_AUIPC){
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op1, inst_op.op2),
          List(True, rd.asUInt, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), io.inst_addr_i.asBits,io.inst_i(31 downto 12) ## Repeat(B(0,1 bits),12)))
      }
      is(INST_NOP){
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o),
          List(False, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits)))
      }
      is(INST_FENCE){
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o,inst_op.op1_jump, inst_op.op2_jump),
          List(False, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits),io.inst_addr_i.asBits,B(4,Xlen bits)))
      }
      is(INST_CSR){
        switch(funct_3){
          is(INST_CSRRW, INST_CSRRS, INST_CSRRC){
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o,inst_op.op_csr_we),
              List(True,rd.asUInt,rs1.asUInt, U(0, log2Up(RegNum) bits),True))
          }
          is(INST_CSRRWI, INST_CSRRSI, INST_CSRRCI){
            Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o, inst_op.op_csr_we),
              List(True, rd.asUInt, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), True))
          }
          default{
            error.set()
          }
        }
      }
      default{
        Connect(List(inst_op.op_reg_we, inst_op.op_reg_waddr, io.reg1_raddr_o, io.reg2_raddr_o),
          List(False, U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits), U(0, log2Up(RegNum) bits)))
        error.set()
      }
    }
  }

  inst_op <> io.opcode
  io.error := error
}

object Decode extends App{
  SpinalSystemVerilog(new Decode)
}
