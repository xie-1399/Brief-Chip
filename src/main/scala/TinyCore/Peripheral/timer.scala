package TinyCore.Peripheral

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.29
 * the timer will show the counter running value and set the time interrupt
 * work steps : set the expired time -> open the timer ctrl count and int enable
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

class timer(addrWidth:Int = 32,dataWidth:Int = 32) extends PrefixComponent{
  require(addrWidth > 4)
  val io = new Bundle{
    val addr = in UInt(addrWidth bits)
    val wr = in Bool()
    val dataIn = in Bits(dataWidth bits)
    val value = out Bits(dataWidth bits)
    val interrupt = out Bool()
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
    when(counter.value > timer_expired.asUInt){
      timer_ctrl(2) := True /* the interrupt is pending */
      counter.clear()
    }
  }.otherwise{
    counter.clear()
  }


  /* write the timer expired and timer ctrl */
  val writeRegs = new Area {
    when(io.wr){
      switch(io.addr(3 downto 0)){
        is(REG_CTRL){
          timer_ctrl := io.dataIn
        }
        is(REG_EXPIRED){
          timer_expired := io.dataIn
        }
      }
    }
  }

  /* read the output value using the diff addr (com logic) */
  io.value := io.addr(3 downto 0).mux(
    REG_EXPIRED -> timer_expired,
    REG_CTRL -> timer_ctrl,
    REG_COUNT -> counter.value.asBits,
    default -> B(0,dataWidth bits)
  )
  io.interrupt := timer_ctrl(1) && timer_ctrl(2)
}


object timer extends App{
  SpinalSystemVerilog(new timer())
}