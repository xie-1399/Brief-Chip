package TinyCore.Core.Excute

import spinal.core._
import spinal.lib._
import TinyCore.Core.Decode._
import TinyCore.Core.Constant.Defines._
import Common.SpinalTools._

class SimpleMulDivPlugin() extends PrefixComponent {
  import ALU._

  val io = new Bundle {
    val valid = in Bool()
    val alu = in(ALU())
    val op1 = in Bits (Xlen bits)
    val op2 = in Bits (Xlen bits)
    val res = out Bits (Xlen bits)
  }
  val low = 31 downto 0
  val high = 63 downto 32

  /* the mul switch */
  val opSigned = io.alu.mux(
    MUL -> (B"11"),
    MULH -> (B"11"),
    MULHSU -> (B"10"),
    MULHU -> (B"00"),
    default -> (B"00")
  )

  val mulop1 = ((opSigned.lsb ? io.op1.msb | False) ## io.op1).asSInt
  val mulop2 = ((opSigned.msb ? io.op2.msb | False) ## io.op2).asSInt
  val temp = (mulop1 * mulop2)
  val lowRes = temp(low).asBits
  val highRes = temp(high).asBits

  val result = io.alu.mux(
    MUL -> lowRes,
    (MULH,MULHSU,MULHU) -> highRes,
    DIVU -> (io.op1.asUInt / io.op2.asUInt).asBits,
    DIV -> (io.op1.asSInt / io.op2.asSInt).asBits,
    REMU -> (io.op1.asUInt % io.op2.asUInt).asBits,
    REM -> (io.op1.asSInt % io.op2.asSInt).asBits,
    default -> io.op1
  )

  when(io.valid) {
    io.res := result
  }.otherwise {
    io.res := B(0, Xlen bits)
  }

}
