package TinyCore.Core

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite.AxiLite4

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.8
 * using the AXI4 as the fetch bus to get the instruction
 * =======================================================
 */
import Defines._
case class FetchL1Cmd() extends Bundle{
  val address = UInt(InstBusAddrWidth bits)
}

case class FetchL1Rsp() extends Bundle{
  val address = UInt(InstBusAddrWidth bits)
  val error = Bool()
  val data = Bits(InstBusDataWidth bits)
}

case class FetchL1Bus() extends Bundle with IMasterSlave{
  val cmd = Stream(FetchL1Cmd())
  val rsp = Stream(FetchL1Rsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toAxi4():Axi4ReadOnly = {
    val axi = Axi4ReadOnly(Parameters.fetchAxi4Config)
    val address = RegNextWhen(cmd.address,cmd.fire).init(CPUReset)

    axi.ar.valid := cmd.valid
    axi.ar.payload.addr := cmd.payload.address
    axi.ar.id := 0
    axi.ar.size := log2Up(InstBusDataWidth / 8)

    cmd.ready := axi.ar.ready
    rsp.valid := axi.r.valid
    rsp.payload.address := address
    rsp.payload.data := axi.r.payload.data
    rsp.error := !axi.r.isOKAY()
    axi.r.ready := True
    axi
  }

}


class FetchAxi4 extends PrefixComponent{

  //ready for the hold at all time
  val io = new Bundle{
    val jtagReset = in Bool()
    val jump = in Bool()
    val jumpAddr = in UInt (InstBusAddrWidth bits)
    val hold = in UInt (HoldWidth bits)

    val axiBus = master (Axi4ReadOnly(Parameters.fetchAxi4Config))
    val inst_o = out Bits (InstBusDataWidth bits)
    val inst_addr_o = out UInt (InstBusAddrWidth bits)
    val decode_valid  = out Bool()
  }

  val pcModule = new PC_Control
  val fetchHold = Reg(UInt(HoldWidth bits)).init(0)
  val holdIt = Mux(fetchHold > io.hold,fetchHold,io.hold)
  /* connect the PC module */
  pcModule.io.hold := holdIt
  pcModule.io.jumpAddr := io.jumpAddr
  pcModule.io.jump := io.jump
  pcModule.io.jtagReset := io.jtagReset

  /* send the bus request out */
  val fetchBus = FetchL1Bus()
  val fetchModule = new Fetch
  when(fetchBus.cmd.fire){
    fetchHold := Hold_Fetch  /* block */
  }.elsewhen(fetchBus.rsp.fire){
    fetchHold := 0  /* release */
  }

  fetchBus.cmd.address := pcModule.io.pcOut
  fetchBus.cmd.valid := pcModule.io.fetchValid
  fetchBus.rsp.ready := True

  /* connect the Fetch module */
  fetchModule.io.hold := io.hold
  fetchModule.io.inst_i := fetchBus.rsp.payload.data
  fetchModule.io.inst_addr_i := fetchBus.rsp.payload.address
  fetchModule.io.fetchInValid := fetchBus.rsp.fire
  io.decode_valid := fetchModule.io.fetchOutValid
  io.inst_o := fetchModule.io.inst_o
  io.inst_addr_o := fetchModule.io.inst_addr_o

  io.axiBus << fetchBus.toAxi4()
}

