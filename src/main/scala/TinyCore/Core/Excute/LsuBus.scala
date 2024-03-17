package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant._
import Defines._
import Parameters._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba3.apb._

import scala.collection.mutable
import scala.util.Random


case class MemoryCmd() extends Bundle with IMasterSlave {
  val valid = Bool()
  val write = Bool()
  val address = UInt(MemAddrBus bits)
  val data = Bits(MemBus bits)
  val mask = Bits(MemBusMask bits)

  override def asMaster(): Unit = {
    out(write,address,data,mask,valid)
  }

  def init() = {
    this.valid := False
    this.write := False
    this.address := 0
    this.data := 0
    this.mask := 0
  }
}

case class LsuPeripheralBusCmd() extends Bundle{
  val write = Bool()
  val address = UInt(MemAddrBus bits)
  val data = Bits(MemBus bits)
  val mask = Bits(MemBusMask bits)
}

case class LsuPeripheralBusRsp() extends Bundle{
  val error = Bool()
  val data = Bits(MemBus bits)
}

case class LsuPeripheralBus() extends Bundle with IMasterSlave{
  val cmd = Stream(LsuPeripheralBusCmd())
  val rsp = Flow(LsuPeripheralBusRsp())
  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def toApb3():Apb3 = {
    val apb = Apb3(MemAddrBus,MemBus)
    val state = RegInit(False)
    when(!state) {
      state := cmd.valid
    }.otherwise {
      when(apb.PREADY) {
        state := False
      }
    }
    apb.PSEL(0) := cmd.valid
    apb.PWRITE := cmd.write
    apb.PADDR := cmd.payload.address
    apb.PWDATA := cmd.payload.data

    cmd.ready := True
    rsp.valid := apb.PENABLE && apb.PSEL(0) && apb.PREADY && !apb.PWRITE
    rsp.payload.data := apb.PRDATA
    rsp.error := apb.PSLVERROR
    apb
  }
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

case class SplitBus() extends PrefixComponent{

  val io = new Bundle{
    val memoryCmd = slave(MemoryCmd())
    val dBus = master(DataMemBus())
    val peripheralBus = master(LsuPeripheralBus())
  }

  val Arbitration = new Area{
    /* is also work if using the Stream fork */
    val cmdFork = Array.fill(2){MemoryCmd()}
    cmdFork.foreach(_.init())
    val selPeripheral = io.memoryCmd.address(31 downto 28) === IoRange
    val seldBus = io.memoryCmd.address(31 downto 28) >= MemoryRange
    when(io.memoryCmd.valid){
      when(seldBus){
        cmdFork(0) <> io.memoryCmd
      }.elsewhen(selPeripheral){
        cmdFork(1) <> io.memoryCmd
      }
    }
    io.dBus.read.cmd.valid := cmdFork(0).valid && !cmdFork(0).write
    io.dBus.read.cmd.payload.address := cmdFork(0).address
    io.dBus.read.rsp.ready := True

    io.dBus.write.cmd.valid := cmdFork(0).valid && cmdFork(0).write
    io.dBus.write.cmd.payload.data := cmdFork(0).data
    io.dBus.write.cmd.payload.address := cmdFork(0).address
    io.dBus.write.cmd.payload.mask := cmdFork(0).mask
    io.dBus.write.rsp.ready := True

    io.peripheralBus.cmd.valid := cmdFork(1).valid
    io.peripheralBus.cmd.payload.address := cmdFork(1).address
    io.peripheralBus.cmd.payload.write := cmdFork(1).write
    io.peripheralBus.cmd.payload.data := cmdFork(1).data
    io.peripheralBus.cmd.payload.mask := cmdFork(1).mask
  }
}

/* the address split it can work */
object SplitBus extends App{
  import spinal.core.sim._
  import Common._
  SIMCFG().compile{
    val dut = new SplitBus()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.memoryCmd.valid #= false
      dut.clockDomain.waitSampling(5)
      var index = 0
      def check(IO:Boolean,write:Boolean,data:Int,mask:Int) = {
        if(IO){
          assert(dut.io.peripheralBus.cmd.write.toBoolean == write ,"peripheralBus write error")
          assert(dut.io.peripheralBus.cmd.data.toBigInt == data,"peripheralBus data error")
          assert(dut.io.peripheralBus.cmd.mask.toBigInt == mask,"peripheralBus mask error")
        }else{
          if(write){
            assert(dut.io.dBus.write.cmd.payload.data.toBigInt == data, "dBus data error")
            assert(dut.io.dBus.write.cmd.payload.mask.toBigInt == mask, "dBus mask error")
          }
        }
      }
      val Check = fork{
        while(index < 100){
          /* only write has mask */
          val randomSeed = Random.nextInt(10) > 5
          val data = Random.nextInt(1024) * 1024
          val mask = Random.nextInt(16)
          val write = Random.nextInt(10) > 5
          dut.io.memoryCmd.valid #= true
          dut.io.memoryCmd.write #= write
          dut.io.memoryCmd.data #= data
          dut.io.memoryCmd.mask #= mask
          if(randomSeed){
            dut.io.memoryCmd.address #= 0x10000000l + Random.nextInt(10) * 1024
            dut.clockDomain.waitSampling()
            check(IO = true, write, data, mask)
          }else{
            dut.io.memoryCmd.address #= 0x80000000l + Random.nextInt(100) * 1024
            dut.clockDomain.waitSampling()
            check(IO = false, write, data, mask)
          }
          index += 1
        }
      }
      Check.join()
      simSuccess()
  }

}