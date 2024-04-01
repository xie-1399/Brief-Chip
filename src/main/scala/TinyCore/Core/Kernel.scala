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
import spinal.lib.bus.amba3.apb._
import spinal.lib.bus.amba4.axi._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.22
 * RISC-V 3-stages Kernel(no need the asyncReset in fact )
 * =======================================================
 */

class Kernel(whitebox:Boolean = false) extends PrefixComponent{
  val io = new Bundle{
    val jtagReset = in Bool()
    val axi4 = master (Axi4(kernelAxi4Config))
    val apb = master (Apb3(kernelApb3Config))
  }

  val core = new Area {
    /* connect the fetch + decode + excute + regfile*/
    val regfile = new Regfile()
    val fetch = new FetchAxi4()
    val decode = new DecodeV2(decodeConfig)
    val excute = new Excute()
    val ctrl = new Ctrl()
    val csr = new CSRPlugin()

    /* connect the pipeline */
    fetch.io.fetchOutPipe <> decode.io.decodeInPipe
    excute.io.excuteInPipe <> decode.io.decodeOutPipe
    excute.io.opcode <> decode.io.decodeSignals
    excute.io.regs <> decode.io.reg
    decode.io.rfread <> regfile.io.read
    excute.io.rfwrite <> regfile.io.write
    excute.io.csrSignals <> csr.io.csrSignals
    fetch.io.jumpOp <> excute.io.jumpOp

    /* ctrl the jump and exception */
    ctrl.io.hold_ex := excute.io.holdEx
    ctrl.io.fetchError := fetch.io.error
    ctrl.io.decodeError := decode.io.error
    ctrl.io.excuteError := excute.io.error
    csr.io.Exception := ctrl.io.Exception
    csr.io.TimeInterrupt := False
    csr.io.SoftwareInterrupt := False
    csr.io.ExternalInterrupt := False

    fetch.io.hold := ctrl.io.holdOut
    decode.io.hold := ctrl.io.holdOut
    fetch.io.flush := ctrl.io.flush
    decode.io.flush := ctrl.io.flush
    ctrl.io.jumpOp <> excute.io.jumpOp

    /* jtag port */
    regfile.io.jtagPort.init()
    fetch.io.jtagReset := io.jtagReset

    val axiCrossBar = Axi4CrossbarFactory()
    axiCrossBar.addSlaves(io.axi4 -> (0x80000000l, 2 GiB))
    axiCrossBar.addConnections(
      fetch.io.iBus.toAxi4() -> List(io.axi4),
      excute.io.dBus.toAxi4() -> List(io.axi4)
    )
    /* add pipeline later */
    axiCrossBar.addPipelining(io.axi4)((readCrossbar, highspeedBus) => {
      readCrossbar.readCmd >> highspeedBus.readCmd
      readCrossbar.readRsp << highspeedBus.readRsp
    })((writeCrossbar, highspeedBus) => {
      writeCrossbar.writeData >> highspeedBus.writeData
      writeCrossbar.writeCmd >> highspeedBus.writeCmd
      writeCrossbar.writeRsp << highspeedBus.writeRsp
    })
    axiCrossBar.build()
    excute.io.peripheralBus.toApb3() >> io.apb

    val whiteBox = whitebox.generate{
      new Area {
        /* for the debug use get the last stage pc */
        val lastStagePC = Reg(UInt(Xlen bits)).init(0)
        lastStagePC.simPublic()
        when(excute.io.excuteInPipe.valid && excute.io.opcode.illegal) {
          lastStagePC := excute.io.excuteInPipe.pc
        }
      }
    }
  }
}

object Kernel extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Kernel)
}