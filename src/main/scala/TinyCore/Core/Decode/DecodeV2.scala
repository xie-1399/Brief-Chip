package TinyCore.Core.Decode

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.10
 * a more complex decode to balance the excute
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant.Instruction._
import TinyCore.Core.Constant.Defines._
import TinyCore.Core.Untils._
import TinyCore.Core.Excute._
import TinyCore.Core.Pipeline.pipeSignals
import TinyCore.Utils._
import spinal.lib._

case class decodeParameters(withRVI: Boolean = true,
                            withRVC: Boolean = false,
                            withRVM: Boolean = true,
                            withRVA: Boolean = false,
                            withRVF: Boolean = false,
                            withCsr: Boolean = true,
                            throwIllegal: Boolean = true)

/* the branch type has it's own encoding format*/
object BR extends SpinalEnum {
  val N, NE, EQ, GE, GEU, LT, LTU, J, JR = newElement()
  defaultEncoding = SpinalEnumEncoding("branch")(
    EQ -> 0,
    NE -> 1,
    J -> 2,
    JR -> 3,
    LT -> 4, //less(<)
    GE -> 5, //grater(>=)
    LTU -> 6,
    GEU -> 7,
    N -> 8 // Not a branch
  )
}

/* the alu type instruction here */
object ALU extends SpinalEnum {
  val ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND, SUB, SRA, COPY = newElement()
  val MUL, MULH, MULHSU, MULHU, DIV, DIVU, REM, REMU = newElement() /* add for the Mul and Div operation */
  defaultEncoding = SpinalEnumEncoding("alu")(
    ADD -> 0,
    SLL -> 1,
    SLT -> 2,
    SLTU -> 3,
    XOR -> 4,
    SRL -> 5,
    OR -> 6,
    AND -> 7,
    SUB -> (8 + 0),
    MUL -> (9),
    MULH -> 10,
    MULHSU -> 11,
    MULHU -> 12,
    SRA -> (8 + 5),
    COPY -> 15,
    DIV -> 16,
    DIVU -> 17,
    REM -> 18,
    REMU -> 19
  )

}

/* memory operation at here */
object MemoryOp extends SpinalEnum {
  val LOAD, LOAD_U, STORE, NOT = newElement()
  defaultEncoding = SpinalEnumEncoding("memory")(
    NOT -> 0,
    LOAD -> 1,
    LOAD_U -> 2,
    STORE -> 3
  )
}

object Mask extends SpinalEnum {
  val WORD, HALF, BYTE = newElement()
  defaultEncoding = SpinalEnumEncoding("mask")(
    WORD -> 15,
    HALF -> 3,
    BYTE -> 1
  )
}

object OP1 extends SpinalEnum(binarySequential) {
  val NOT, RS1, PC = newElement()
}

object OP2 extends SpinalEnum(binarySequential) {
  val NOT, IMM_I, IMM_S, IMM_B, IMM_J, IMM_U, RS2 = newElement()
}

object CSR extends SpinalEnum(binarySequential) {
  val N, C, S, W = newElement()
}

case class regSignals() extends Bundle with IMasterSlave {
  val reg1_rdata_o = Bits(RegWidth bits)
  val reg2_rdata_o = Bits(RegWidth bits)
  val reg_we = Bool()
  val reg_waddr = UInt(RegNumLog2 bits)

  override def asMaster(): Unit = {
    out(reg1_rdata_o, reg2_rdata_o, reg_we, reg_waddr)
  }
}

case class CtrlSignals(p: decodeParameters) extends Bundle {

  import p._

  val illegal = Bool()
  val jump = Bool()
  val fencei = Bool()
  val op1 = OP1()
  val op2 = OP2()
  val mask = Mask()
  val branch = BR()
  val alu = ALU()
  val memoryOption = MemoryOp()
  val csr = CSR()
  /* default : Seq(N,N,N,N,N,N,N,U(0,5 bits),U(0,5 bits),U(0,5 bits),BR.N,ALU.COPY,MemoryOp.NOT,CSR.N) */
}

object DecodeConstant {
  val rs1Range = 19 downto 15
  val rs2Range = 24 downto 20
  val rdRange = 11 downto 7
  val opcodeRange = 6 downto 0
}

class DecodeV2(p: decodeParameters) extends PrefixComponent {

  import DecodeConstant._

  val io = new Bundle {
    val decodeInPipe = slave(pipeSignals())
    val decodeOutPipe = master(pipeSignals())
    val hold = in UInt (HoldWidth bits)
    val flush = in Bool()
    val decodeSignals = out(CtrlSignals(p))
    val error = out Bool()
    val reg = master(regSignals())
    /* to the ex */
    /* to the regfile */
    val rfread = master(rfRead())
  }

  def Y = True

  def N = False

  val ctrl = CtrlSignals(p)
  val reg = regSignals()
  /* switch of decode*/
  val opcode = io.decodeInPipe.inst(opcodeRange)
  val rs1 = io.decodeInPipe.inst(rs1Range).asUInt
  val rs2 = io.decodeInPipe.inst(rs2Range).asUInt
  val rd = io.decodeInPipe.inst(rdRange).asUInt
  val funct_3 = io.decodeInPipe.inst(14 downto 12)
  val funct_7 = io.decodeInPipe.inst(31 downto 25)
  val holdDecode = io.hold >= Hold_Decode
  val error = RegInit(False).setWhen(io.decodeInPipe.valid && !ctrl.illegal)
  assignBundleWithList(ctrl, Seq(N, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.N))
  reg.reg_we := False
  /* the decode valid drive it */
  when(io.decodeInPipe.valid) {
    /* Todo other inst to add */
    when(io.decodeInPipe.inst === INST_MRET){
      assignBundleWithList(ctrl, Seq(Y, N, Y, OP1.NOT, OP2.NOT, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.N))
    }
    switch(opcode) {
      is(INST_TYPE_I) {
        reg.reg_we.set()
        switch(funct_3) {
          is(INST_ADDI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_XORI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.XOR, MemoryOp.NOT, CSR.N))
          }
          is(INST_ORI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.OR, MemoryOp.NOT, CSR.N))
          }
          is(INST_ANDI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.AND, MemoryOp.NOT, CSR.N))
          }
          is(INST_SLLI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLL, MemoryOp.NOT, CSR.N))
          }
          is(INST_SRI) {
            when(funct_7 === 32) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SRA, MemoryOp.NOT, CSR.N))
            }.elsewhen(funct_7 === 0) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SRL, MemoryOp.NOT, CSR.N))
            }
          }
          is(INST_SLTI) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLT, MemoryOp.NOT, CSR.N))
          }
          is(INST_SLTIU) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLTU, MemoryOp.NOT, CSR.N))
          }
        }
      }
      is(INST_TYPE_R_M) {
        when(funct_7 === 0 || funct_7 === 32) {
          reg.reg_we.set()
          switch(funct_3) {
            is(INST_ADD_SUB) {
              when(funct_7 === 0) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.ADD, MemoryOp.NOT, CSR.N))
              }.elsewhen(funct_7 === 32) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SUB, MemoryOp.NOT, CSR.N))
              }
            }
            is(INST_XOR) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.XOR, MemoryOp.NOT, CSR.N))
            }
            is(INST_OR) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.OR, MemoryOp.NOT, CSR.N))
            }
            is(INST_AND) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.AND, MemoryOp.NOT, CSR.N))
            }
            is(INST_SLL) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SLL, MemoryOp.NOT, CSR.N))
            }
            is(INST_SR) {
              when(funct_7 === 0) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SRL, MemoryOp.NOT, CSR.N))
              }.elsewhen(funct_7 === 32) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SRA, MemoryOp.NOT, CSR.N))
              }
            }
            is(INST_SLT) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SLT, MemoryOp.NOT, CSR.N))
            }
            is(INST_SLTU) {
              assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.SLTU, MemoryOp.NOT, CSR.N))
            }
          }
        }.elsewhen(funct_7 === 1) {
          p.withRVM.generate {
            reg.reg_we.set()
            switch(funct_3) {
              is(INST_MUL) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MUL, MemoryOp.NOT, CSR.N))
              }
              is(INST_MULH) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULH, MemoryOp.NOT, CSR.N))
              }
              is(INST_MULHU) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULHU, MemoryOp.NOT, CSR.N))
              }
              is(INST_MULHSU) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULHSU, MemoryOp.NOT, CSR.N))
              }
              is(INST_DIV) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.DIV, MemoryOp.NOT, CSR.N))
              }
              is(INST_DIVU) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.DIVU, MemoryOp.NOT, CSR.N))
              }
              is(INST_REM) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.REM, MemoryOp.NOT, CSR.N))
              }
              is(INST_REMU) {
                assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.REMU, MemoryOp.NOT, CSR.N))
              }
            }
          }
        }
      }
      is(INST_TYPE_L) {
        switch(funct_3) {
          is(INST_LB) {
            reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.BYTE, BR.N, ALU.ADD, MemoryOp.LOAD, CSR.N))
          }
          is(INST_LH) {
            reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.HALF, BR.N, ALU.ADD, MemoryOp.LOAD, CSR.N))
          }
          is(INST_LW) {
            reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.ADD, MemoryOp.LOAD, CSR.N))
          }
          is(INST_LBU) {
            reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.BYTE, BR.N, ALU.ADD, MemoryOp.LOAD_U, CSR.N))
          }
          is(INST_LHU) {
            reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_I, Mask.HALF, BR.N, ALU.ADD, MemoryOp.LOAD_U, CSR.N))
          }
        }
      }
      is(INST_TYPE_S) {
        switch(funct_3) {
          is(INST_SB) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_S, Mask.BYTE, BR.N, ALU.ADD, MemoryOp.STORE, CSR.N))
          }
          is(INST_SH) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_S, Mask.HALF, BR.N, ALU.ADD, MemoryOp.STORE, CSR.N))
          }
          is(INST_SW) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_S, Mask.WORD, BR.N, ALU.ADD, MemoryOp.STORE, CSR.N))
          }
        }
      }
      is(INST_TYPE_B) {
        switch(funct_3) {
          is(INST_BEQ) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.EQ, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_BNE) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.NE, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_BLT) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.LT, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_BGE) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.GE, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_BLTU) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.LTU, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
          is(INST_BGEU) {
            assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_B, Mask.WORD, BR.GEU, ALU.ADD, MemoryOp.NOT, CSR.N))
          }
        }
      }
      is(INST_JAL) {
        reg.reg_we.set()
        assignBundleWithList(ctrl, Seq(Y, Y, N, OP1.PC, OP2.IMM_J, Mask.WORD, BR.J, ALU.ADD, MemoryOp.NOT, CSR.N))
      }
      is(INST_JALR) {
        reg.reg_we.set()
        assignBundleWithList(ctrl, Seq(Y, Y, N, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.JR, ALU.ADD, MemoryOp.NOT, CSR.N))
      }
      is(INST_LUI) {
        reg.reg_we.set()
        assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_U, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.N))
      }
      is(INST_AUIPC) {
        reg.reg_we.set()
        assignBundleWithList(ctrl, Seq(Y, N, N, OP1.PC, OP2.IMM_U, Mask.WORD, BR.N, ALU.ADD, MemoryOp.NOT, CSR.N))
      }

      is(INST_NOP) {
        /* insert with nop */
        assignBundleWithList(ctrl, Seq(Y, N, N, OP1.NOT, OP2.NOT, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.N))
      }
      is(INST_FENCE) {
        assignBundleWithList(ctrl, Seq(Y, N, Y, OP1.NOT, OP2.NOT, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.N))
      }

      is(INST_CSR) {
        p.withCsr.generate {
          switch(funct_3) {
            is(INST_CSRRW) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.W))
            }
            is(INST_CSRRS) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.S))
            }
            is(INST_CSRRC) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.C))
            }
            is(INST_CSRRWI) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_U, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.W))
            }
            is(INST_CSRRSI) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_U, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.S))
            }
            is(INST_CSRRCI) {
              reg.reg_we.set(); assignBundleWithList(ctrl, Seq(Y, N, N, OP1.RS1, OP2.IMM_U, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT, CSR.C))
            }
          }

        }
      }
    }
  }

  /* with one stage pipe out */
  val dff_decode = new Pipe_DFF(io.decodeInPipe.getBitsWidth)
  dff_decode.io.din.assignFromBits(io.decodeInPipe.asBits)
  dff_decode.io.hold := holdDecode || error || io.flush
  dff_decode.io.default := 0
  io.decodeOutPipe.assignFromBits(dff_decode.io.dout)

  val dff_decodeSignals = new Pipe_DFF(ctrl.getBitsWidth)
  dff_decodeSignals.io.din.assignFromBits(ctrl.asBits)
  dff_decodeSignals.io.hold := holdDecode || error || io.flush
  dff_decodeSignals.io.default := B(0, ctrl.getBitsWidth bits)
  io.decodeSignals.assignFromBits(dff_decodeSignals.io.dout)
  io.error := error

  /* send the reg cmd to the regfile*/
  reg.reg_waddr := rd
  io.rfread.Rs1Addr := rs1
  io.rfread.Rs2Addr := rs2
  reg.reg1_rdata_o := io.rfread.Rs1Data
  reg.reg2_rdata_o := io.rfread.Rs2Data
  val dff_reg = new Pipe_DFF(reg.getBitsWidth)
  dff_reg.io.din.assignFromBits(reg.asBits)
  dff_reg.io.hold := holdDecode || error || io.flush
  dff_reg.io.default := B(0, reg.getBitsWidth bits)
  io.reg.assignFromBits(dff_reg.io.dout)
}

/* stop it with enum string will be better */
object DecodeV2 extends App {
  SpinalVerilog(SpinalConfig().withoutEnumString())(new DecodeV2(decodeParameters()))
}