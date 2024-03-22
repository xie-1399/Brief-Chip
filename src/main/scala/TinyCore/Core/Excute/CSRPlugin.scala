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
 * the CSR needs to control the interrupt
 * =======================================================
 */

case class CsrSignals() extends Bundle with IMasterSlave {

  val csr_we = Bool()
  val csr_waddr = in UInt()

  override def asMaster(): Unit = {

  }


}

class CSRPlugin extends PrefixComponent{

  val io = new Bundle{
    /* the excute stage read and write the CSR*/
    val csr_we = in Bool()
    val csr_raddr = in UInt(MemAddrBus bits)
    val  = in UInt(MemAddrBus bits)
    val csr_wdata = in Bits(MemBus bits)
    val csr_rdata = out Bits(MemBus bits)

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

  val csrWAddr = io.csr_waddr(11 downto 0)
  val csrRAddr = io.csr_raddr(11 downto 0)
  val globalInt_en = mstatus(3) === True  /* the global interrupt is open or not */

  val csrInst = new Area{
    /* the csr inst will read or write the csr regs here */
    when(io.csr_we){
      switch(csrWAddr){
        is(CSR_MIE){mie := io.csr_wdata}
        is(CSR_MTVEC){mtvec := io.csr_wdata}
        is(CSR_MCAUSE){mcause := io.csr_wdata}
        is(CSR_MEPC){mepc := io.csr_wdata}
        is(CSR_MSCRATCH){mscratch := io.csr_wdata}
        is(CSR_MSTATUS){mstatus := io.csr_wdata}
        //nothing todo for the other csrs
      }
    }

    when(io.csr_we && csrWAddr === csrRAddr){
      io.csr_rdata := io.csr_wdata
    }.otherwise{
      switch(csrRAddr){
        is(CSR_MIE) {io.csr_rdata := mie}
        is(CSR_MTVEC) {io.csr_rdata := mtvec}
        is(CSR_MCAUSE) { io.csr_rdata := mcause}
        is(CSR_MEPC) {io.csr_rdata := mepc}
        is(CSR_MSCRATCH) {io.csr_rdata := mscratch}
        is(CSR_MSTATUS) {io.csr_rdata := mstatus}
        is(CSR_CYCLE){io.csr_rdata := cycles(31 downto 0).asBits}
        is(CSR_CYCLEH){io.csr_rdata := cycles(63 downto 32).asBits}
        default{io.csr_rdata := 0}
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