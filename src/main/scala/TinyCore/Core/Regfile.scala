package TinyCore.Core

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.21
 * RISC-V reg file get 32 regs and read and write about it seems ready
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

class Regfile extends PrefixComponent{
  // jtag and decode stage will send the reg file cmd
  import Defines._
  val io = new Bundle{
    /* write the reg from ex */
    val we_i = in Bool()
    val waddr_i = in UInt(RegNumLog2 bits)
    val wdata_i = in Bits(RegWidth bits)

    /* write the reg from jtag */
    val jtag_we_i = in Bool()
    val jtag_addr_i = in UInt (RegNumLog2 bits)
    val jtag_data_i = in Bits (RegWidth bits)
    val jtag_data_o = out Bits(RegWidth bits)
    /* read */
    val raddr1_i = in UInt(RegNumLog2 bits)
    val rdata1_o = out Bits (RegWidth bits)

    val raddr2_i = in UInt(RegNumLog2 bits)
    val rdata2_o = out Bits(RegWidth bits)
  }

  val regfile = Mem(Bits(RegWidth bits),RegNum)
  private val registerNames = Seq("zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0_fp", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
  )

  /* debug it */
  val whiteBox = new Area {
    for(idx <- 0 until RegNum){
      val regWire = Bits(RegWidth bits)
      regWire.setName(s"x${idx}")
      regWire := regfile.readAsync(U(idx).resized)
    }
  }

  val write = new Area{
    /* the ex comes first */
    when(io.we_i && io.waddr_i =/= 0){
      regfile.write(io.waddr_i,io.wdata_i)
    }.elsewhen(io.jtag_we_i && io.jtag_addr_i =/= 0){
      regfile.write(io.jtag_addr_i,io.jtag_data_i)
    }
  }

  val read = new Area{
    when(io.raddr1_i === 0){
      io.rdata1_o := 0
    }.elsewhen(io.raddr1_i === io.waddr_i && io.we_i){
      io.rdata1_o := io.wdata_i
    }.otherwise {
      io.rdata1_o := regfile.readAsync(io.raddr1_i)
    }

    when(io.raddr2_i === 0) {
      io.rdata2_o := 0
    }.elsewhen(io.raddr2_i === io.waddr_i && io.we_i) {
      io.rdata2_o := io.wdata_i
    }.otherwise {
      io.rdata2_o := regfile.readAsync(io.raddr2_i)
    }
    when(io.jtag_addr_i === 0){
      io.jtag_data_o := 0
    }.otherwise{
      io.jtag_data_o := regfile.readAsync(io.jtag_addr_i)
    }
  }

}

object Regfile extends App{
  SpinalSystemVerilog(new Regfile)
}
