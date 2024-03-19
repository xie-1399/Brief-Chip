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
import spinal.lib.bus.amba3.apb._

class Apb3Gpio(addrWidth:Int = 32,dataWidth:Int = 32,gpioWidth:Int = 2) extends PrefixComponent{
  val io = new Bundle{
    val apb = slave (Apb3(addrWidth,dataWidth))
    val gpi = in Bits(gpioWidth bits)
    val gpo = out Bits(gpioWidth bits)
  }
  require(gpioWidth <= 16)

  def GPIO_CTRL = U(0,4 bits)
  def GPIO_DATA = U(4,4 bits)

  val gpio_ctrl = Reg(Bits(dataWidth bits)) .init(0)
  val gpio_data = Reg(Bits(gpioWidth bits)) .init(0)
  val error = RegInit(False)

  /* first config the ctrl and data regs*/
  /* using the ctrl -> 1 : output 2:input */
  val writeIt = io.apb.PSEL(0) && io.apb.PWRITE && io.apb.PENABLE && io.apb.PREADY

  when(writeIt){
    when(io.apb.PADDR(3 downto 0) === GPIO_CTRL){
      gpio_ctrl := io.apb.PWDATA
    }.elsewhen(io.apb.PADDR(3 downto 0) === GPIO_DATA){
      gpio_data := io.apb.PWDATA(gpioWidth - 1 downto 0)
    }.otherwise{
      error.set()
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

  io.apb.PRDATA := io.apb.PADDR(3 downto 0).mux(
    GPIO_DATA -> gpio_data.resized,
    GPIO_CTRL -> gpio_ctrl,
    default -> B(0,dataWidth bits)
  )
  io.gpo := gpio_data
  io.apb.PSLVERROR := error
  io.apb.PREADY := True
}

/* Todo with test goes run */
object Apb3Gpio extends App{
  SpinalVerilog(new Apb3Gpio(32,32,2))
}