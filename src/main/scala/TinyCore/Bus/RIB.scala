package TinyCore.Bus

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * the rib bus is in the tiny riscv project(a set of masters and slaves)
 * source : https://github.com/liangkangnan/tinyriscv/blob/master/rtl/core/rib.v
 * =======================================================
 */

import Common.SIMCFG
import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import Common.SimUntils._

/* maybe trans it works better (all out ports should be reg ) */

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

/* all the combine logic (seems not good ) */
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
  val grant = Bits(2 bits)
  val hold = RegInit(False)
  val HighSel = MemAddrBus - 1 downto MemAddrBus - 4  /* the high 4 bits used to choose which slave */
  /* fixed priority 0 > 1 > 2 > 3 */
  when(sels.asUInt > 0){hold := True}

  /* the grant logic */
  when(io.sels(0)){
    grant := B"00"
  }.elsewhen(io.sels(1)){
    grant := B"01"
  }.elsewhen(io.sels(2)){
    grant := B"10"
  }.elsewhen(io.sels(3)){
    grant := B"11"
  }.otherwise{
    grant := B"11"
  }

  /* init the out */
  for(idx <- 0 until SlaveNum){
    io.slaves(idx).wdata := 0
    io.slaves(idx).wr := False
    io.slaves(idx).addr := 0
  }
  for(idx <- 0 until MasterNum){
    io.masters(idx).rdata := 0
  }

  /* combine logic to ari the master cmd to the really slave*/
  switch(grant){
    is(B"00"){
      switch(io.masters(0).addr(HighSel)){
        is(slave_0){io.masters(0) <> io.slaves(0)}
        is(slave_1){io.masters(0) <> io.slaves(1)}
        is(slave_2){io.masters(0) <> io.slaves(2)}
        is(slave_3){io.masters(0) <> io.slaves(3)}
        is(slave_4){io.masters(0) <> io.slaves(4)}
      }
    }

    is(B"01"){
      switch(io.masters(1).addr(HighSel)) {
        is(slave_0) {io.masters(1) <> io.slaves(0)}
        is(slave_1) {io.masters(1) <> io.slaves(1)}
        is(slave_2) {io.masters(1) <> io.slaves(2)}
        is(slave_3) {io.masters(1) <> io.slaves(3)}
        is(slave_4) {io.masters(1) <> io.slaves(4)}
      }
    }

    is(B"10"){
      switch(io.masters(2).addr(HighSel)) {
        is(slave_0) {io.masters(2) <> io.slaves(0)}
        is(slave_1) {io.masters(2) <> io.slaves(1)}
        is(slave_2) {io.masters(2) <> io.slaves(2)}
        is(slave_3) {io.masters(2) <> io.slaves(3)}
        is(slave_4) {io.masters(2) <> io.slaves(4)}
      }
    }

    is(B"11"){
      switch(io.masters(3).addr(HighSel)) {
        is(slave_0) {io.masters(3) <> io.slaves(0)}
        is(slave_1) {io.masters(3) <> io.slaves(1)}
        is(slave_2) {io.masters(3) <> io.slaves(2)}
        is(slave_3) {io.masters(3) <> io.slaves(3)}
        is(slave_4) {io.masters(3) <> io.slaves(4)}
      }
    }
  }
  /* connect the io */
  io.hold := hold
}

// for the rib simulation if necessary
object RIB extends App{
  import spinal.core.sim._

  SIMCFG().compile{
    val dut = new RIB()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)

      def check(slaves:rib,masters:rib): Unit = {
        assert(slaves.rdata.toBigInt == masters.rdata.toBigInt,"rdata not match")
        assert(slaves.wdata.toBigInt == masters.wdata.toBigInt,"wdata not match")
        assert(slaves.wr.toBigInt == masters.wr.toBigInt,"wr not match")
        assert(slaves.addr.toBigInt == masters.addr.toBigInt,"addr not match")
      }

      def masterAccess(masters:rib): Unit = {
        masters.addr.randomize()
        masters.wdata.randomize()
        masters.wr.randomize()
      }

      def slaveAccess(slaves:rib) = {
        slaves.rdata.randomize()
      }

      def randomSel(sel:Bool): Unit = {
        sel.randomize()
      }
      def matches(sel:Int,addr:String): Unit = {
        val sub = addr.substring(0,4)

        sub match{
          case "0000" => {check(dut.io.slaves(0),dut.io.masters(sel))}
          case "0001" => {check(dut.io.slaves(1),dut.io.masters(sel))}
          case "0010" => {check(dut.io.slaves(2),dut.io.masters(sel))}
          case "0011" => {check(dut.io.slaves(3),dut.io.masters(sel))}
          case "0100" => {check(dut.io.slaves(4),dut.io.masters(sel))}
          case  _ =>
        }
      }

      /* the master send the request randomly */
      dut.io.sels(0) #= false
      dut.io.sels(1) #= false
      dut.io.sels(2) #= false
      dut.io.sels(3) #= false
      dut.io.masters.foreach(master => masterAccess(master))
      dut.io.slaves.foreach(slave => slaveAccess(slave))
      dut.clockDomain.waitSampling()

      val masterCmd = fork{
        var matches = 0
        while (true){
          dut.io.sels.foreach(sel => randomSel(sel))
          dut.io.masters.foreach(master => masterAccess(master))
          dut.clockDomain.waitSampling()
          matches += 1
          if(matches == 600) simSuccess()
        }
      }
      dut.clockDomain.onSamplings{
        if(dut.io.sels(0).toBoolean){matches(0,StringAdapter(dut.io.masters(0).addr.toBigInt.toString(2),32))}
        else if(dut.io.sels(1).toBoolean){matches(1,StringAdapter(dut.io.masters(1).addr.toBigInt.toString(2),32))}
        else if(dut.io.sels(2).toBoolean){matches(2,StringAdapter(dut.io.masters(2).addr.toBigInt.toString(2),32))}
        else if(dut.io.sels(3).toBoolean){matches(3,StringAdapter(dut.io.masters(3).addr.toBigInt.toString(2),32))}
      }
      masterCmd.join() /* block it */
      simSuccess()
  }

}
