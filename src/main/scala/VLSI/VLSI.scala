package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.28
 * the CNN Accelerator top module
 *=======================================================
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

/* using the apb bus to driver some regs internal
* 0x0 -> start conv   0x4 -> conv end */

class VLSI extends Component {
  val io = new Bundle{
    val clk = in Bool()
    val reset = in Bool()
    // val apb = slave(Apb3(32,32))
    val weight = in Bits(32 bits)
    val ifm = in Bits(64 bits)
  }

  /* clock domain and reset ctrl */
  val AsyncResetClockDomain = ClockDomain(
    clock = io.clk,
    config = ClockDomainConfig(resetKind = BOOT)
  )
  val resetCtrl = new ClockingArea(AsyncResetClockDomain){
    val AsyncReset = RegNext(io.reset)
  }

  val systemClockDomain = ClockDomain(
    clock = io.clk,
    reset = resetCtrl.AsyncReset,
    frequency = FixedFrequency(50 MHz)
  )

  // val rows = Vec()

  /* PE Array */




}
