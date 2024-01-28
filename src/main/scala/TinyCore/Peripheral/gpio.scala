package TinyCore.Peripheral

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the gpio is used as a simple inout unit to control something
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

class gpio(addrWidth:Int = 32,dataWidth:Int = 32,gpioWidth:Int = 2) extends PrefixComponent{
  val io = new Bundle{
    val wr = in Bool()
    val addr = in UInt(addrWidth bits)
    val dataIn = in Bits(dataWidth bits) /* used to write the the regs in the GPIO module*/
    val gpi = in Bits(gpioWidth bits)
    val gpo = out Bits(gpioWidth bits)
    val dataOut = out Bits(dataWidth bits) /* used to read the regs in the GPIO module */
  }
  require(gpioWidth <= 16)

  def GPIO_CTRL = U(0,4 bits)
  def GPIO_DATA = U(4,4 bits)

  val gpio_ctrl = Reg(Bits(dataWidth bits)) .init(0)
  val gpio_data = Reg(Bits(gpioWidth bits)) .init(0)

  /* first config the ctrl and data regs*/
  /* using the ctrl -> 1 : output 2:input */

  when(io.wr){
    when(io.addr(3 downto 0) === GPIO_CTRL){
      gpio_ctrl := io.dataIn
    }.elsewhen(io.addr(3 downto 0) === GPIO_DATA){
      gpio_data := io.dataIn(gpioWidth - 1 downto 0)
    }
  }.otherwise{
    /* using the 2 bits control one */
    val ctrls = gpio_ctrl(gpioWidth * 2 - 1 downto 0).asBools.grouped(2)
    ctrls.zipWithIndex.foreach (ctrl =>
        when(ctrl._1.asBits() === B"10"){
          gpio_data(ctrl._2) := io.gpi(ctrl._2)
        }
    )
  }

  io.dataOut := io.addr(3 downto 0).mux(
    GPIO_DATA -> gpio_data.resized,
    GPIO_CTRL -> gpio_ctrl,
    default -> B(0,dataWidth bits)
  )
  io.gpo := gpio_data
}

object gpio extends App{
  SpinalVerilog(new gpio(32,32,16))
}