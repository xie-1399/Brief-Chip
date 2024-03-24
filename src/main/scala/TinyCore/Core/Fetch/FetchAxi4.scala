package TinyCore.Core.Fetch

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import TinyCore.Core.Constant._
import Defines._
import Instruction._
import TinyCore.Core.Constant.Parameters.fetchAxi4Config
import TinyCore.Core.Excute.JumpOp
import TinyCore.Core.Pipeline.pipeSignals
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.8
 * using the AXI4 as the fetch bus to get the instruction
 * =======================================================
 */

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
    val axi = Axi4ReadOnly(fetchAxi4Config)
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
    val jumpOp = slave(JumpOp())
    val hold = in UInt (HoldWidth bits)
    val flush = in Bool()
    val error = out Bool()
    val iBus = master (FetchL1Bus())
    val fetchOutPipe = master(pipeSignals())
  }

  val pcModule = new PC_Control
  val fetchHold = Reg(UInt(HoldWidth bits)).init(0)
  val holdIt = Mux(fetchHold > io.hold,fetchHold,io.hold)
  /* connect the PC module */
  pcModule.io.hold := holdIt
  pcModule.io.jumpOp <> io.jumpOp
  pcModule.io.jtagReset := io.jtagReset

  /* send the bus request out */
  val fetchBus = FetchL1Bus()
  val fetchModule = new Fetch
  when(fetchBus.cmd.fire){
    fetchHold := Hold_Fetch  /* block */
  }.elsewhen(fetchBus.rsp.fire){
    fetchHold := 0  /* release */
  }

  val throwIt = RegInit(False).setWhen(io.flush).clearWhen(fetchBus.rsp.fire)
  fetchBus.cmd.address := pcModule.io.pcOut.payload
  fetchBus.cmd.valid := pcModule.io.pcOut.valid
  pcModule.io.pcOut.ready := fetchBus.cmd.ready
  fetchBus.rsp.ready := True

  /* connect the Fetch module */
  fetchModule.io.hold := io.hold
  fetchModule.io.fetchInPipe.inst := fetchBus.rsp.payload.data
  fetchModule.io.fetchInPipe.pc := fetchBus.rsp.payload.address
  fetchModule.io.fetchInPipe.valid := Mux(io.hold >= Hold_Fetch,False,fetchBus.rsp.fire) && !throwIt
  io.fetchOutPipe <> fetchModule.io.fetchOutPipe
  io.iBus <> fetchBus

  io.error := fetchBus.rsp.error
}

