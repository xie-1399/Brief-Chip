package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant._
import Defines._
import Parameters._
import spinal.lib.bus.amba4.axi._


/* this is about the Peripheral */
case class LsuPeripheralBusCmd() extends Bundle{

}

case class LsuPeripheralBusRsp() extends Bundle{

}

case class LsuPeripheralBus() extends Bundle{

}


/* the data memory read signals*/
case class DataMemReadCmd() extends Bundle{
  val address = UInt(MemAddrBus bits)
}
case class DataMemReadRsp() extends Bundle{
  val data = Bits(MemBus bits)
  val error = Bool()
}
case class DataMemReadBus() extends Bundle with IMasterSlave{
  val cmd = Stream(DataMemReadCmd())
  val rsp = Stream(DataMemReadRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

case class DataMemWriteCmd() extends Bundle{
  val address = UInt(MemAddrBus bits)
  val data = Bits(MemBus bits)
  val mask = Bits(MemBusMask bits) /* 4 bit mask */
}
case class DataMemWriteRsp() extends Bundle{
  val error = Bool()
}
case class DataMemWriteBus() extends Bundle with IMasterSlave{
  val cmd = Stream(DataMemWriteCmd())
  val rsp = Stream(DataMemWriteRsp())

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }
}

/* read / write the memory data */
case class DataMemBus() extends Bundle with IMasterSlave{
  val read = DataMemReadBus()
  val write = DataMemWriteBus()
  override def asMaster(): Unit = {
    master(read,write)
  }

  /* to the axi bus request*/
  def toAxi4():Axi4 = {
    val axi = Axi4(memoryAxi4Config)

    axi.ar.valid := read.cmd.valid
    axi.ar.payload.addr := read.cmd.address
    axi.ar.payload.size := log2Up(MemBus / 8)
    axi.ar.payload.id := 0
    read.cmd.ready := axi.ar.ready
    read.rsp.valid := axi.r.valid
    read.rsp.payload.data := axi.r.payload.data
    read.rsp.payload.error := !axi.r.isOKAY()
    axi.r.ready := True

    axi.aw.valid := write.cmd.valid
    axi.aw.payload.addr := write.cmd.address
    axi.aw.payload.id := 0
    axi.aw.payload.size := log2Up(MemBus / 8)
    write.cmd.ready := axi.aw.ready

    //axi.w.ready := True
    val wValid = RegNext(axi.aw.fire).init(False)
    axi.w.valid := wValid
    axi.w.strb := write.cmd.mask
    axi.w.data := write.cmd.data
    axi.w.last := True

    axi.b.ready := True
    write.rsp.valid := axi.b.valid
    write.rsp.error := !axi.b.isOKAY()
    axi
  }
}
