package TinyCore.Core

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import TinyCore.Core.Fetch._
import TinyCore.Core.Decode._
import TinyCore.Core.Constant.Parameters._
import TinyCore.Core.Constant.Defines._
import TinyCore.Core.Excute._
import spinal.lib.bus.amba4.axi._
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.22
 * RISC-V 3-stages Kernel
 * =======================================================
 */

class Kernel extends PrefixComponent{
  val io = new Bundle{
    val clk = in Bool()
    val reset = in Bool()
    val jtagReset = in Bool()
    val axi4 = master (Axi4(kernelAxi4Config))
  }

  val AsyncResetClockDomain = ClockDomain(
    clock = io.clk,
    config = ClockDomainConfig(resetKind = BOOT,resetActiveLevel = LOW)
  )
  val resetCtrl = new ClockingArea(AsyncResetClockDomain) {
    val AsyncReset = RegNext(io.reset) simPublic()
  }

  val systemClockDomain = ClockDomain(
    clock = io.clk,
    reset = resetCtrl.AsyncReset,
    frequency = FixedFrequency(100 MHz)
  )

  val core = new ClockingArea(systemClockDomain){
    /* connect the fetch + decode + excute + regfile*/
    val regfile = new Regfile()
    val fetchAxi4 = new FetchAxi4()
    val decode = new DecodeV2(decodeConfig)
    val excute = new Excute()
    val ctrl = new Ctrl()
    /* connect the components */

    regfile.io.jtag_we_i := False
    regfile.io.jtag_addr_i := 0
    regfile.io.jtag_data_i := 0

    fetchAxi4.io.hold := ctrl.io.holdOut
    fetchAxi4.io.jump := False
    fetchAxi4.io.jumpAddr := 0
    fetchAxi4.io.jtagReset := io.jtagReset

    decode.io.inst_addr_i := fetchAxi4.io.inst_addr_o
    decode.io.inst := fetchAxi4.io.inst_o
    decode.io.valid_i := fetchAxi4.io.decode_valid
    decode.io.hold := ctrl.io.holdOut

    decode.io.reg1_rdata := regfile.io.rdata1_o
    decode.io.reg2_rdata := regfile.io.rdata2_o

    regfile.io.raddr1_i := decode.io.reg1_raddr
    regfile.io.raddr2_i := decode.io.reg2_raddr

    excute.io.opcode <> decode.io.decodeSignals
    excute.io.ex_valid := decode.io.valid_o
    excute.io.inst_i := decode.io.inst_o
    excute.io.inst_addr_i := decode.io.inst_addr_o
    excute.io.regs <> decode.io.reg

    ctrl.io.hold_ex := excute.io.holdEx
    ctrl.io.stageId := 0
    ctrl.io.stageError := 0

    regfile.io.we_i := excute.io.reg_we_o
    regfile.io.waddr_i := excute.io.reg_waddr_o
    regfile.io.wdata_i := excute.io.reg_wdata_o

    val axiCrossBar = Axi4CrossbarFactory()
    axiCrossBar.addSlaves(io.axi4 -> (0x80000000L, 2 GiB))
    axiCrossBar.addConnections(
      fetchAxi4.io.axiBus -> List(io.axi4),
      excute.io.axiBus -> List(io.axi4)
    )
    axiCrossBar.addPipelining(io.axi4)((readCrossbar, highspeedBus) => {
      readCrossbar.readCmd >> highspeedBus.readCmd
      readCrossbar.readRsp << highspeedBus.readRsp
    })((writeCrossbar, highspeedBus) => {
      writeCrossbar.writeData >> highspeedBus.writeData
      writeCrossbar.writeCmd >> highspeedBus.writeCmd
      writeCrossbar.writeRsp << highspeedBus.writeRsp
    })
    axiCrossBar.build()

    val whiteBox = new Area {
      /* for the debug use get the last stage pc */
      val lastStagePC = Reg(UInt(Xlen bits)).init(0)
      lastStagePC.simPublic()
      when(excute.io.ex_valid && excute.io.opcode.illegal) {
        lastStagePC := excute.io.inst_addr_i
      }
    }
  }
}

object Kernel extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Kernel)
}