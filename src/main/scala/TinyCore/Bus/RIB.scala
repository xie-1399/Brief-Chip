package TinyCore.Bus

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * the rib bus is in the tiny riscv project(a set of masters and slaves)
 * source : https://github.com/liangkangnan/tinyriscv/blob/master/rtl/core/rib.v
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

/* maybe trans it works better */

case class rib(addrWidth:Int = 32,dataWidth:Int = 32) extends Bundle with IMasterSlave {
  val addr = UInt(addrWidth bits)
  val wdata = Bits(dataWidth bits)
  val rdata = Bits(dataWidth bits)
  val wr = Bool()

  override def asMaster(): Unit = {
    out(wdata,addr,wr)
    in(rdata)
  }
}

/* pipe with the cycles going */

class RIB() extends PrefixComponent{
  /* 4 masters send to the 5 slaves works*/

  import TinyCore.Core.Defines._

  val io = new Bundle{
    val masters = Vec(slave(rib(MemAddrBus,MemBus)),MasterNum)
    val sels = in Vec(Bool(),MasterNum)  /* which device is using the bus */
    val slaves = Vec(master(rib(MemAddrBus,MemBus)),SlaveNum)
    val hold = out Bool()
  }

  /* using the high 4 bits to choose the slaves */
  val sels = io.sels.asBits
  val grant = Reg(Bits(2 bits)).init(0)
  val hold = RegInit(False)

  /* fixed priority 0 > 1 > 2 > 3 */
  when(sels.asUInt > 0){
    hold := True
  }

  /* fix the mux bugs */

  def getSlave(addr:UInt) = {
    val slave_sel = addr.mux(
      slave_0 -> io.slaves(0),
      slave_1 -> io.slaves(1),
      slave_2 -> io.slaves(2),
      slave_3 -> io.slaves(3),
      slave_4 -> io.slaves(4)
    )
    slave_sel
  }

  val HighSel = MemAddrBus - 1 downto MemAddrBus - 4  /* the high 4 bits used to choose which slave */

  for(idx <- 0 until MasterNum){
    io.masters(idx).rdata := B(0,MemBus bits)
  }
  for(idx <- 0 until SlaveNum){
    io.slaves(idx).addr := 0
    io.slaves(idx).wdata := 0
    io.slaves(idx).wr := False
  }

  /* combine logic */
  switch(grant){
    is(B"00"){
      io.masters(0) <> getSlave(io.masters(0).addr(HighSel))
    }

    is(B"01"){
      io.masters(1) <> getSlave(io.masters(1).addr(HighSel))
    }

    is(B"10"){
      io.masters(2) <> getSlave(io.masters(2).addr(HighSel))
    }

    is(B"11"){
      io.masters(3) <> getSlave(io.masters(3).addr(HighSel))
    }
  }
  /* connect the io */
  io.hold := hold
}

object RIB extends App{
  SpinalVerilog(new RIB())
}