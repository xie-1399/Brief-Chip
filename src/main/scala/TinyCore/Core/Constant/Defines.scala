package TinyCore.Core.Constant

import spinal.core._
import spinal.lib.bus.amba4.axi._
import TinyCore.Core.Decode._
import spinal.lib.bus.amba3.apb.Apb3Config
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
  def IoRange = U(1,4 bits)
  def MemoryRange = U(8,4 bits)
  def Xlen = 32
  def stageNum = 3

  /* hold the pipeline defines */
  def HoldWidth = 2
  def Hold_PC: UInt = U(1, HoldWidth bits)
  def Hold_Fetch: UInt = U(2, HoldWidth bits)
  def Hold_Decode: UInt = U(3, HoldWidth bits)
  /* about Bus */
  def InstBusDataWidth = 32
  def InstBusAddrWidth = 32
  def CsrAddrWidth = 12
  def CsrMemWidth = 32
  def MemBus = 32
  def MemAddrBus = 32
  def MemBusMask = 4

  /* common regs */
  def RegWidth = 32
  def RegNum = 32
  def RegNumLog2 = 5
}

object Instruction{
  /* the instruction format can be listed at the material */

  def INST_NOP: Bits = B(1, 7 bits) /* not do anything */

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
  def MVENDORID = 0xF11 // MRO Vendor ID.
  def MARCHID = 0xF12 // MRO Architecture ID.
  def MIMPID = 0xF13 // MRO Implementation ID.
  def MHARTID = 0xF14 // MRO Hardware thread ID.Machine Trap Setup
  def MSTATUS = 0x300 // MRW Machine status register.
  def MISA = 0x301 // MRW ISA and extensions
  def MEDELEG = 0x302 // MRW Machine exception delegation register.
  def MIDELEG = 0x303 // MRW Machine interrupt delegation register.
  def MIE = 0x304 // MRW Machine interrupt-enable register.
  def MTVEC = 0x305 // MRW Machine trap-handler base address. Machine Trap Handling
  def MSCRATCH = 0x340 // MRW Scratch register for machine trap handlers.
  def MEPC = 0x341 // MRW Machine exception program counter.
  def MCAUSE = 0x342 // MRW Machine trap cause.
  def MBADADDR = 0x343 // MRW Machine bad address.
  def MIP = 0x344 // MRW Machine interrupt pending.
  def MBASE = 0x380 // MRW Base register.
  def MBOUND = 0x381 // MRW Bound register.
  def MIBASE = 0x382 // MRW Instruction base register.
  def MIBOUND = 0x383 // MRW Instruction bound register.
  def MDBASE = 0x384 // MRW Data base register.
  def MDBOUND = 0x385 // MRW Data bound register.
  def MCYCLE = 0xB00 // MRW Machine cycle counter.
  def MINSTRET = 0xB02 // MRW Machine instructions-retired counter.
  def MCYCLEH = 0xB80 // MRW Upper 32 bits of mcycle, RV32I only.
  def MINSTRETH = 0xB82 // MRW Upper 32 bits of minstret, RV32I only.
  val MCOUNTEREN = 0x306
}


object Parameters{
  import Defines._
  /* use size and last , resp show error, and support the burst later */
  def fetchAxi4Config = Axi4Config(addressWidth = InstBusAddrWidth, dataWidth = InstBusDataWidth, idWidth = 4, useRegion = false,
    useBurst = false, useLock = false, useCache = false, useSize = true, useQos = false, useLen = false, useLast = true, useResp = true,
    useProt = false, useStrb = false)

  def decodeConfig = decodeParameters()  /* with Csr and IM instruction */

  val memoryAxi4Config = Axi4Config(addressWidth = MemAddrBus, dataWidth = MemBus, idWidth = 4, useRegion = false,
    useBurst = false, useLock = false, useCache = false, useSize = true, useQos = false, useLen = false, useLast = true, useResp = true,
    useProt = false, useStrb = true)

  val kernelAxi4Config = Axi4Config(addressWidth = MemAddrBus, dataWidth = MemBus, idWidth = 5, useRegion = false,
    useBurst = false, useLock = false, useCache = false, useSize = true, useQos = false, useLen = false, useLast = true, useResp = true,
    useProt = false, useStrb = true)

  val kernelApb3Config = Apb3Config(addressWidth = MemAddrBus, dataWidth = MemBus)

  /* Peripheral Config */
  def RomSize = 4 KiB
  def RamSize = 16 KiB
}
