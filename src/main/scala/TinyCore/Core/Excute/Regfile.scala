package TinyCore.Core.Excute

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.21
 * RISC-V reg file get 32 regs and read and write about it seems ready
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import TinyCore.Core.Constant._
import Defines._
import TinyCore.Core.Debug.JtagPort

case class rfRead() extends Bundle with IMasterSlave{
  val Rs1Addr = UInt(RegNumLog2 bits)
  val Rs1Data = Bits(RegWidth bits)
  val Rs2Addr = UInt(RegNumLog2 bits)
  val Rs2Data = Bits(RegWidth bits)
  override def asMaster(): Unit = {
    out(Rs1Addr,Rs2Addr)
    in(Rs1Data,Rs2Data)
  }
}

case class rfWrite() extends Bundle with IMasterSlave{
  val waddr = UInt(RegNumLog2 bits)
  val we = Bool()
  val wdata = Bits(RegWidth bits)
  override def asMaster(): Unit = {
   out(wdata,we,waddr)
  }
}

class Regfile(debug:Boolean = false) extends PrefixComponent {
  // jtag and decode stage will send the reg file cmd
  val io = new Bundle {
    /* write the reg from ex */
    val write = slave(rfWrite())
    /* read */
    val read = slave(rfRead())
    /* jtag port*/
    val jtagPort = slave(JtagPort())
  }

  val regfile = Mem(Bits(RegWidth bits), RegNum)
  private val registerNames = Seq("zero", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0_fp", "s1", "a0", "a1", "a2", "a3", "a4",
    "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5", "t6"
  )

  /* debug it */
  val whiteBox = debug.generate {
    new Area {
      for (idx <- 0 until RegNum) {
        val regWire = Bits(RegWidth bits)
        regWire.setName(s"x${idx}")
        regWire := regfile.readAsync(U(idx).resized)
      }
    }
  }

  val write = new Area {
    /* the ex comes first (the order should be carefully ) -> last wins */
    regfile.write(io.jtagPort.jtag_Addr, io.jtagPort.jtag_Wdata, enable = io.jtagPort.jtag_We && io.jtagPort.jtag_Addr =/= 0)
    regfile.write(io.write.waddr, io.write.wdata, enable = io.write.we && io.write.waddr =/= 0)
  }

  val read = new Area {
    val rs1 = regfile.readAsync(io.read.Rs1Addr)
    val rs2 = regfile.readAsync(io.read.Rs2Addr)
    val jtagData = regfile.readAsync(io.jtagPort.jtag_Addr)

    io.read.Rs1Data := io.read.Rs1Addr.mux(
      U(0, RegNumLog2 bits) -> B(0, RegWidth bits),
      io.write.waddr -> Mux(io.write.we, io.write.wdata, rs1),
      default -> rs1
    )

    io.read.Rs2Data := io.read.Rs2Addr.mux(
      U(0, RegNumLog2 bits) -> B(0, RegWidth bits),
      io.write.waddr -> Mux(io.write.we, io.write.wdata, rs2),
      default -> rs2
    )

    io.jtagPort.jtag_Rdata := Mux(io.jtagPort.jtag_Addr === 0, B(0).resized, jtagData)
  }
}

object Regfile extends App{
  SpinalSystemVerilog(new Regfile)
}
