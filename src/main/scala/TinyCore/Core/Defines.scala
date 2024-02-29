package TinyCore.Core

import spinal.core._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * the constant for the cpu config and the decode Inst type
 * =======================================================
 */


object Defines{

  /* cpu top */
  def CPUReset = 0x80000000l
  def Xlen = 32
  def JumpEnable: Bool = True
  def JumpDisable: Bool = False


  /* hold the pipeline defines */
  def HoldEnable: Bool = True
  def HoldDisable: Bool = False
  def HoldWidth = 3
  def Hold_None: UInt = U(0, HoldWidth bits)
  def Hold_PC: UInt = U(1, HoldWidth bits)
  def Hold_Fetch: UInt = U(2, HoldWidth bits)

  /* about Bus */
  def InstBusDataWidth = 32
  def InstBusAddrWidth = 32
  def MemBus = 32
  def MemAddrBus = 32
  def MasterNum = 4
  def SlaveNum = 5
  def slave_0 = U(0,4 bits)
  def slave_1 = U(1,4 bits)
  def slave_2 = U(2,4 bits)
  def slave_3 = U(3,4 bits)
  def slave_4 = U(4,4 bits)



  /* some constant value */
  def ZeroWord: Bits = B(0, InstBusDataWidth bits)

  /* common regs */
  def RegAddrBus = 4 downto 0
  def RegBus = 31 downto 0
  def DoubleRegBus =  63 downto 0
  def RegWidth = 32
  def RegNum = 32        // reg num
  def RegNumLog2 = 5
}

object Instruction{
  /* the instruction format can be listed at the material */

  def INST_NOP: Bits = B(19, 32 bits) /* 0x00000013 (00010011) addi x0,x0,0 */

  /* define the default inst */
  def INST_DEFAULT:Bits = B(1,32 bits)
  def INST_DEFAULT_OP:Bits = B(1, 7 bits)

  // I type inst
  def INST_TYPE_I = B(19,7 bits) //7'b0010011
  def INST_ADDI = B(0,3 bits)
  def INST_SLTI = B(2,3 bits)
  def INST_SLTIU = B(3,3 bits)
  def INST_XORI = B(4,3 bits)
  def INST_ORI = B(6,3 bits)
  def INST_ANDI = B(7,3 bits)
  def INST_SLLI = B(1,3 bits)
  def INST_SRI = B(5,3 bits)

  // L type inst
  def INST_TYPE_L = B(3,7 bits) // 7'b0000011
  def INST_LB = B(0,3 bits)  // 3'b000
  def INST_LH = B(1,3 bits)  //3'b001
  def INST_LW = B(2,3 bits)  //3'b010
  def INST_LBU = B(4,3 bits) //3'b100
  def INST_LHU = B(5,3 bits) //3'b101

  // S type inst
  def INST_TYPE_S = B(35,7 bits) //7'b0100011
  def INST_SB = B(0,3 bits)      // 3'b000
  def INST_SH = B(1,3 bits)      // 3'b001
  def INST_SW = B(2,3 bits)      //3'b010

  // R and M type inst
  def INST_TYPE_R_M = B(51,7 bits)  //7'b0110011
  // R type inst
  def INST_ADD_SUB = B(0,3 bits) // 3'b000
  def INST_SLL = B(1,3 bits) // 3'b001
  def INST_SLT = B(2,3 bits) // 3'b010
  def INST_SLTU = B(3,3 bits) //3'b011
  def INST_XOR = B(4,3 bits) //3'b100
  def INST_SR = B(5,3 bits)  //3'b101
  def INST_OR = B(6,3 bits)  //3'b110
  def INST_AND = B(7,3 bits) //3'b111
  // M type inst
  def INST_MUL = B(0,3 bits) //3'b000
  def INST_MULH = B(1,3 bits)   //3'b001
  def INST_MULHSU = B(2,3 bits)  //3'b010
  def INST_MULHU = B(3,3 bits)  //3'b011
  def INST_DIV = B(4,3 bits) // 3'b100
  def INST_DIVU = B(5,3 bits) //3'b101
  def INST_REM = B(6,3 bits) //3'b110
  def INST_REMU = B(7,3 bits) //3'b111

  // J type inst
  def INST_JAL = B(111,7 bits) //7'b1101111
  def INST_JALR = B(103,7 bits) //7'b1100111

  def INST_LUI = B(55,7 bits) // 7'b0110111
  def INST_AUIPC = B(23,7 bits) // 7'b0010111
  def INST_MRET = B(0x30200073,32 bits) //32'h30200073
  def INST_RET = B(0x00008067,32 bits) //32'h00008067

  def INST_FENCE = B(15,7 bits) // 7'b0001111
  def INST_ECALL = B(0x00000073,32 bits) // 32'h73
  def INST_EBREAK = B(0x00100073,32 bits) // 32'h00100073

  // B type inst
  def INST_TYPE_B = B(0x63,7 bits) // 7'b1100011
  def INST_BEQ = B(0,3 bits) // 3'b000
  def INST_BNE = B(1,3 bits) // 3'b001
  def INST_BLT = B(4,3 bits) //3'b100
  def INST_BGE = B(5,3 bits) // 3'b101
  def INST_BLTU = B(6,3 bits) //3'b110
  def INST_BGEU = B(7,3 bits) // 3'b111

  // CSR inst
  def INST_CSR = B(0x73,7 bits) // 7'b1110011
  def INST_CSRRW = B(1,3 bits) // 3'b001
  def INST_CSRRS = B(2,3 bits) // 3'b010
  def INST_CSRRC = B(3,3 bits) //3'b011
  def INST_CSRRWI = B(5,3 bits) //3'b101
  def INST_CSRRSI = B(6,3 bits) // 3'b110
  def INST_CSRRCI = B(7,3 bits) // 3'b111

  // some using CSR reg addr
  def CSR_CYCLE = 0xc00
  def CSR_CYCLEH =0xc80
  def CSR_MTVEC = 0x305
  def CSR_MCAUSE = 0x342
  def CSR_MEPC = 0x341
  def CSR_MIE = 0x304
  def CSR_MSTATUS = 0x300
  def CSR_MSCRATCH = 0x340
}
