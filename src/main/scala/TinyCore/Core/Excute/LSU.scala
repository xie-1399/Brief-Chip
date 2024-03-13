package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant._
import Defines._


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
  val mask = Bits(MemBusMask bits) /* 4 bit mask */
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

}
