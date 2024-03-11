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

case class decodeParameters(withRVI:Boolean = true,
                            withRVC:Boolean = false,
                            withRVM:Boolean = true,
                            withRVA:Boolean = false,
                            withRVF:Boolean = false,
                            withCsr:Boolean = true,
                            throwIllegal:Boolean = true)

/* the branch type has it's own encoding format*/
object BR extends SpinalEnum{
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
object ALU extends SpinalEnum{
  val ADD, SLL, SLT, SLTU, XOR, SRL, OR, AND, SUB, SRA, COPY = newElement()
  val MUL,MULH,MULHSU,MULHU,DIV,DIVU,REM,REMU = newElement() /* add for the Mul and Div operation */
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
object MemoryOp extends SpinalEnum{
  val LOAD,LOAD_U,STORE,NOT = newElement()
  defaultEncoding = SpinalEnumEncoding("memory")(
    NOT -> 0,
    LOAD -> 1,
    LOAD_U -> 2,
    STORE -> 3
  )
}

object Mask extends SpinalEnum{
  val WORD,HALF,BYTE = newElement()
  defaultEncoding = SpinalEnumEncoding("mask")(
    WORD -> 15,
    HALF -> 3,
    BYTE -> 1
  )
}

object OP1 extends SpinalEnum(binarySequential){
  val RS1,PC = newElement()
}
object OP2 extends SpinalEnum(binarySequential){
  val IMM_I,IMM_S,IMM_B,IMM_J,IMM_U,RS2 = newElement()
}

case class csrSignals() extends Bundle{ /* to ex and to csr regs */
    val csr_rdata = in Bits(RegWidth bits)
    val csr_raddr = out UInt(RegWidth bits)
    val csr_we = out Bool()
    val csr_waddr = out UInt(MemAddrBus bits)
}

case class CtrlSignals(p:decodeParameters) extends Bundle {
  import p._
  val illegal = Bool()
  val useRs1 = Bool()
  val useRs2 = Bool()
  val useRd = Bool()
  val jump = Bool()
  val fencei = Bool()
  val compress = Bool()
  val rs1 = UInt(5 bits)
  val rs2 = UInt(5 bits)
  val rd = UInt(5 bits)
  val op1 = OP1()
  val op2 = OP2()
  val mask = Mask()
  val branch = BR()
  val alu = ALU()
  val memoryOption = MemoryOp()
  /* default : Seq(N,N,N,N,N,N,N,U(0,5 bits),U(0,5 bits),U(0,5 bits),BR.N,ALU.COPY,MemoryOp.NOT) */
}

object DecodeConstant{
  val rs1Range = 19 downto 15
  val rs2Range = 24 downto 20
  val rdRange = 11 downto 7
  val opcodeRange = 6 downto 0
}

class DecodeV2(p:decodeParameters) extends PrefixComponent{
  import DecodeConstant._
  val io = new Bundle {
    val inst = in Bits (32 bits)
    val valid = in Bool()
    val decodeOut = out(CtrlSignals(p))
    val error = out Bool()
    val csr = ifGen(p.withCsr){csrSignals()}
  }

  def Y = True
  def N = False
  val ctrl = CtrlSignals(p)
  /* switch of decode*/
  val opcode = io.inst(opcodeRange)
  val rs1 = io.inst(rs1Range).asUInt
  val rs2 = io.inst(rs2Range).asUInt
  val rd = io.inst(rdRange).asUInt
  val funct_3 = io.inst(14 downto 12)
  val funct_7 = io.inst(31 downto 25)
  assignBundleWithList(ctrl, Seq(N, N, N, N, N, N, N, U(0, 5 bits), U(0, 5 bits), U(0, 5 bits), OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT))

  p.withCsr.generate {
    io.csr.csr_raddr := io.inst(31 downto 20).asUInt.resized
    io.csr.csr_waddr := io.inst(31 downto 20).asUInt.resized
    io.csr.csr_we := False
  }
  /* the decode valid drive it */
  when(io.valid){
    switch(opcode){
      is(INST_TYPE_I){
        switch(funct_3){
          is(INST_ADDI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.ADD, MemoryOp.NOT))}
          is(INST_XORI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.XOR, MemoryOp.NOT))}
          is(INST_ORI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.OR, MemoryOp.NOT))}
          is(INST_ANDI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.AND, MemoryOp.NOT))}
          is(INST_SLLI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLL, MemoryOp.NOT))}
          is(INST_SRI) {
            when(funct_7 === 32){assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SRA, MemoryOp.NOT))
            }.elsewhen(funct_7 === 0){assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SRL, MemoryOp.NOT))}
          }
          is(INST_SLTI) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLT, MemoryOp.NOT))}
          is(INST_SLTIU) {
            assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.SLTU, MemoryOp.NOT))
          }
        }
      }
      is(INST_TYPE_R_M){
        when(funct_7 === 0 || funct_7 === 32) {
          switch(funct_3){
            is(INST_ADD_SUB) {
              when(funct_7 === 0 ){
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.ADD, MemoryOp.NOT))
              }.elsewhen(funct_7 === 32){
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SUB, MemoryOp.NOT))
              }
            }
            is(INST_XOR) {assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.XOR, MemoryOp.NOT))}
            is(INST_OR) {assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.OR, MemoryOp.NOT))}
            is(INST_AND) {assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.AND, MemoryOp.NOT))}
            is(INST_SLL) {assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SLL, MemoryOp.NOT))}
            is(INST_SR) {
              when(funct_7 === 0){
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SRL, MemoryOp.NOT))
              }.elsewhen(funct_7 === 32){
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SRA, MemoryOp.NOT))
              }
            }
            is(INST_SLT) {assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SLT, MemoryOp.NOT))}
            is(INST_SLTU) {assignBundleWithList(ctrl,Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.SLTU, MemoryOp.NOT))}
          }
        }.elsewhen(funct_7 === 1){
          p.withRVM.generate {
            switch(funct_3) {
              is(INST_MUL) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MUL, MemoryOp.NOT))
              }
              is(INST_MULH) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULH, MemoryOp.NOT))
              }
              is(INST_MULHU) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULHU, MemoryOp.NOT))
              }
              is(INST_MULHSU) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.MULHSU, MemoryOp.NOT))
              }
              is(INST_DIV) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.DIV, MemoryOp.NOT))
              }
              is(INST_DIVU) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.DIVU, MemoryOp.NOT))
              }
              is(INST_REM) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.REM, MemoryOp.NOT))
              }
              is(INST_REMU) {
                assignBundleWithList(ctrl, Seq(Y, Y, Y, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.REMU, MemoryOp.NOT))
              }
            }
          }
        }
      }
      is(INST_TYPE_L){
        switch(funct_3){
          is(INST_LB) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.BYTE, BR.N, ALU.ADD, MemoryOp.LOAD))}
          is(INST_LH) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.HALF, BR.N, ALU.ADD, MemoryOp.LOAD))}
          is(INST_LW) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.N, ALU.ADD, MemoryOp.LOAD))}
          is(INST_LBU) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.BYTE, BR.N, ALU.ADD, MemoryOp.LOAD_U))}
          is(INST_LHU) {assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.HALF, BR.N, ALU.ADD, MemoryOp.LOAD_U))}
        }
      }
      is(INST_TYPE_S){
        switch(funct_3){
          is(INST_SB){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_S, Mask.BYTE,BR.N, ALU.ADD, MemoryOp.STORE))}
          is(INST_SH){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_S, Mask.HALF,BR.N, ALU.ADD, MemoryOp.STORE))}
          is(INST_SW){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_S, Mask.WORD,BR.N, ALU.ADD, MemoryOp.STORE))}
        }
      }
      is(INST_TYPE_B){
        switch(funct_3){
          is(INST_BEQ){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.EQ, ALU.ADD, MemoryOp.NOT))}
          is(INST_BNE){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.NE, ALU.ADD, MemoryOp.NOT))}
          is(INST_BLT){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.LT, ALU.ADD, MemoryOp.NOT))}
          is(INST_BGE){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.GE, ALU.ADD, MemoryOp.NOT))}
          is(INST_BLTU){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.LTU, ALU.ADD, MemoryOp.NOT))}
          is(INST_BGEU){assignBundleWithList(ctrl, Seq(Y, Y, Y, N, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_B, Mask.WORD,BR.GEU, ALU.ADD, MemoryOp.NOT))}
        }
      }
      is(INST_JAL){
        assignBundleWithList(ctrl, Seq(Y, N, N, Y, Y, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_J, Mask.WORD,BR.N, ALU.ADD, MemoryOp.NOT))
      }
      is(INST_JALR){
        assignBundleWithList(ctrl, Seq(Y, Y, N, Y, Y, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_I, Mask.WORD, BR.JR, ALU.ADD, MemoryOp.NOT))
      }
      is(INST_LUI){
        assignBundleWithList(ctrl, Seq(Y, N, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.IMM_U, Mask.WORD,BR.N, ALU.COPY, MemoryOp.NOT))
      }
      is(INST_AUIPC){
        assignBundleWithList(ctrl, Seq(Y, N, N, Y, N, N, N, rs1, rs2, rd, OP1.PC, OP2.IMM_U, Mask.WORD,BR.N, ALU.ADD, MemoryOp.NOT))
      }

      is(INST_NOP) { /* insert with nop */
        assignBundleWithList(ctrl, Seq(Y, N, N, N, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD, BR.N, ALU.COPY, MemoryOp.NOT))
      }
      is(INST_FENCE){
        assignBundleWithList(ctrl, Seq(Y, N, N, N, N, Y, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.COPY, MemoryOp.NOT))
      }

      is(INST_CSR){
        p.withCsr.generate{
          switch(funct_3){
            is(INST_CSRRW, INST_CSRRS, INST_CSRRC){
              io.csr.csr_we := True
              assignBundleWithList(ctrl, Seq(Y, Y, N, Y, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.COPY, MemoryOp.NOT))
            }
            is(INST_CSRRWI, INST_CSRRSI, INST_CSRRCI){
              io.csr.csr_we := True
              assignBundleWithList(ctrl, Seq(Y, N, N, N, N, N, N, rs1, rs2, rd, OP1.RS1, OP2.RS2, Mask.WORD,BR.N, ALU.COPY, MemoryOp.NOT))
            }
          }

        }
      }
    }
  }
  io.decodeOut:= ctrl
  io.error := io.valid && !ctrl.illegal
}

object DecodeV2 extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new DecodeV2(decodeParameters()))
}