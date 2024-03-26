package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import TinyCore.Core.Constant._
import Defines._
import Instruction._
import spinal.lib._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.10
 * the CSR needs to control the interrupt（using a state machine to present all）
 * =======================================================
 */

case class CsrSignals() extends Bundle with IMasterSlave {
  val csr_we = Bool()
  val csr_waddr = UInt(CsrAddrWidth bits)
  val csr_wdata = Bits(CsrMemWidth bits)
  val csr_rdata = Bits(CsrMemWidth bits)
  val csr_raddr = UInt(CsrAddrWidth bits)
  override def asMaster(): Unit = {
    out(csr_we,csr_wdata,csr_waddr,csr_raddr)
    in(csr_rdata)
  }
}

class CSRPlugin extends PrefixComponent{

  val io = new Bundle{
    /* the excute stage read and write the CSR*/
    val csrSignals = slave(CsrSignals())
    val TimeInterrupt = in Bool()
    val SoftwareInterrupt = in Bool()
    val ExternalInterrupt = in Bool()
    val Exception = in Bool()
  }


  /* declare the csr regs here */
  val cycles = Reg(UInt(64 bits)).init(0)
  cycles := cycles + 1
  val mtvec = Reg(Bits(MemBus bits)).init(0) /* the interrupt entry */
  val mcause = Reg(Bits(MemBus bits)).init(0) /* the interrupt cause reason */
  val mepc = Reg(Bits(MemBus bits)).init(0) /* the return pc */
  val mie = Reg(Bits(MemBus bits)).init(0) /* the return pc */
  val mstatus = Reg(Bits(MemBus bits)).init(0) /* the return pc */
  val mscratch = Reg(Bits(MemBus bits)).init(0) /* the return pc */

  val globalInt_en = mstatus(3) === True  /* the global interrupt is open or not */

  val csrInst = new Area{
    /* the csr inst will read or write the csr regs here */
    when(io.csrSignals.csr_we){
      switch(io.csrSignals.csr_waddr){
        is(CSR_MIE){mie := io.csrSignals.csr_wdata}
        is(CSR_MTVEC){mtvec := io.csrSignals.csr_wdata}
        is(CSR_MCAUSE){mcause := io.csrSignals.csr_wdata}
        is(CSR_MEPC){mepc := io.csrSignals.csr_wdata}
        is(CSR_MSCRATCH){mscratch := io.csrSignals.csr_wdata}
        is(CSR_MSTATUS){mstatus := io.csrSignals.csr_wdata}
        //nothing todo for the other csrs
      }
    }

    when(io.csrSignals.csr_we && io.csrSignals.csr_raddr === io.csrSignals.csr_waddr){
      io.csrSignals.csr_rdata := io.csrSignals.csr_wdata
    }.otherwise{
      switch(io.csrSignals.csr_raddr){
        is(CSR_MIE) {io.csrSignals.csr_rdata := mie}
        is(CSR_MTVEC) {io.csrSignals.csr_rdata := mtvec}
        is(CSR_MCAUSE) { io.csrSignals.csr_rdata := mcause}
        is(CSR_MEPC) {io.csrSignals.csr_rdata := mepc}
        is(CSR_MSCRATCH) {io.csrSignals.csr_rdata := mscratch}
        is(CSR_MSTATUS) {io.csrSignals.csr_rdata := mstatus}
        is(CSR_CYCLE){io.csrSignals.csr_rdata := cycles(31 downto 0).asBits}
        is(CSR_CYCLEH){io.csrSignals.csr_rdata := cycles(63 downto 32).asBits}
        default{io.csrSignals.csr_rdata := 0}
      }
    }
  }

  val interrupt = new Area{
    /* may using a state machine control it is enough */

  }

}

object CSRPlugin extends App{
  SpinalSystemVerilog(new CSRPlugin)
}