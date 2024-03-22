package TinyCore.Core.Excute

import spinal.core._
import spinal.lib._
import TinyCore.Core.Decode._
import TinyCore.Core.Constant.Defines._
import Common.SpinalTools._

/* rebuild the ALU unit and contains all */

class ALUPlugin extends PrefixComponent{
  /* implement some arithmetic operation */

  import ALU._
  val io = new Bundle{
    val alu = in (ALU())
    val valid = in Bool()
    val op1 = in Bits(Xlen bits)
    val op2 = in Bits(Xlen bits)
    val res = out Bits(Xlen bits)
  }

  /* the imm shift should be low 5 bits*/
  val shiftIMMRange = io.op2(11 downto 5)
  val shiftIMM = shiftIMMRange === 0 || shiftIMMRange === 32
  val shiftOp = Mux(shiftIMM,io.op2(4 downto 0).asUInt,io.op2.asUInt)

  val bitsCal = io.alu.mux(
    AND -> (io.op1 & io.op2),
    OR -> (io.op1 | io.op2),
    XOR -> (io.op1 ^ io.op2),
    SLL -> (io.op1 |<< shiftOp), /* logic shift */
    SRL -> (io.op1 |>> shiftOp),
    SRA -> (io.op1.asSInt >> shiftOp).asBits, /* the arithmetic shift using */
    COPY -> io.op2, /* copy is for the operation 2 */
    default -> io.op1
  )
  val lessU = io.alu === SLTU
  val less = Mux(lessU,io.op1.asUInt < io.op2.asUInt,io.op1.asSInt < io.op2.asSInt)

  val doSub = io.alu === SUB
  val addSub = Mux(doSub,io.op1.asSInt - io.op2.asSInt,io.op1.asSInt + io.op2.asSInt).asBits

  when(io.valid){
    io.res := io.alu.mux(
      (SLT,SLTU) -> less.asBits.resized,
      (ADD,SUB) -> addSub,
      default -> bitsCal
    )
  }.otherwise{
    io.res := B(0,Xlen bits)
  }
}
