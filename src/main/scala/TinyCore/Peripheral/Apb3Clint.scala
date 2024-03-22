package TinyCore.Peripheral

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.29
 * the timer will show the counter running value and set the time interrupt
 * work steps : set the expired time -> open the timer ctrl count and int enable
 * the software interrupt is a reg set
 * =======================================================
 */

import Common.SIMCFG
import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba3.apb.sim.Apb3Driver

/* Todo add the software and time interrupt as the clint goes */
class Apb3Clint(addrWidth:Int = 32,dataWidth:Int = 32) extends PrefixComponent{
  require(addrWidth > 4)
  val io = new Bundle{
    val apb = slave(Apb3(addrWidth,dataWidth))
    val TimerInterrupt = out Bool()
  }
  def CreateCounter(width:Int = 32) = {
    val counter = Counter(BitCount(width)).init(0)
    counter
  }
  def counterBits = 32
  def REG_CTRL = U(0,4 bits)
  def REG_COUNT = U(4,4 bits)
  def REG_EXPIRED = U(8,4 bits)
  val counter = CreateCounter(counterBits)
  val timer_expired = Reg(Bits(dataWidth bits)).init(0)  /* the up time */
  val timer_ctrl = Reg(Bits(dataWidth bits)).init(0)

  /* using the time ctrl to control the timer
  * only when the timer enable : 0x0 [0]
  * the [1] is enable interrupt and [2] is show the pending time interrupt */
  when(timer_ctrl(0)){
    counter.increment()
    when(counter.value >= timer_expired.asUInt){
      timer_ctrl(2) := True /* the interrupt is pending */
    }
  }.otherwise{
    counter.clear()
  }
  /* write the timer expired and timer ctrl */
  val writeRegs = new Area {
    val writeIt = io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE && io.apb.PREADY
    when(writeIt){
      switch(io.apb.PADDR(3 downto 0)){
        is(REG_CTRL){
          timer_ctrl(1 downto 0) := io.apb.PWDATA(1 downto 0)
        }
        is(REG_EXPIRED){
          timer_expired := io.apb.PWDATA
        }
      }
    }
  }
  io.apb.PRDATA := io.apb.PADDR(3 downto 0).mux(
    REG_EXPIRED -> timer_expired,
    REG_CTRL -> timer_ctrl,
    REG_COUNT -> counter.value.asBits,
    default -> B(0,dataWidth bits)
  )
  io.TimerInterrupt := timer_ctrl(1) && timer_ctrl(2)
  io.apb.PSLVERROR := False
  io.apb.PREADY := True
}


object Apb3Clint extends App{
  import spinal.core.sim._
  SIMCFG().compile{
    val dut = new Apb3Clint()
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val driver = new Apb3Driver(dut.io.apb,dut.clockDomain)
      driver.write(0x8,0x100)
      assert(driver.read(0x8) == 0x100)
      driver.write(0x0,0x3) /* ctrl goes */
      assert(driver.read(0x0) == 0x3)
      dut.clockDomain.waitSamplingWhere(dut.io.TimerInterrupt.toBoolean)
      simSuccess()
  }
}