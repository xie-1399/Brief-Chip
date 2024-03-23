package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import TinyCore.Core.Decode._
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant.Parameters._
import TinyCore.Core.Constant.Defines._
import TinyCore.Core.Decode._
import TinyCore.Core.Pipeline.pipeSignals
import spinal.core
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.bus.amba4.axi.Axi4
/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.10
 * Excute stage -> excute all kinds of instruction
 * =======================================================
 */

class Excute extends PrefixComponent{
  val io = new Bundle{
    val opcode = in (CtrlSignals(decodeConfig)) /* using the decode v2*/
    val excuteInPipe = slave(pipeSignals())
    /* from the regfile */
    val regs = slave(regSignals())
    /* write the regs */
    val rfwrite = master(rfWrite())
    val holdEx = out UInt(HoldWidth bits)
    val dBus = master(DataMemBus())
    val peripheralBus = master(LsuPeripheralBus())
  }

  def isMulDiv(ctrl: CtrlSignals): Bool = {
    import ALU._
    val res = ctrl.alu.mux(
      MUL -> True,
      MULH -> True,
      MULHSU -> True,
      MULHU -> True,
      DIV -> True,
      DIVU -> True,
      REMU -> True,
      REM -> True,
      default -> False
    )
    res
  }

  val op1 = Bits(Xlen bits)
  op1 := io.opcode.op1.mux(
    OP1.RS1 -> io.regs.reg1_rdata_o,
    OP1.PC -> io.excuteInPipe.pc.asBits,
    default -> B(0,Xlen bits)
  )
  val op2 = Bits(Xlen bits)
  op2 := io.opcode.op2.mux(
    OP2.RS2 -> io.regs.reg2_rdata_o,
    OP2.IMM_I -> Repeat(io.excuteInPipe.inst.msb,20) ## io.excuteInPipe.inst(31 downto 20),
    OP2.IMM_B -> Repeat(io.excuteInPipe.inst.msb,20) ## io.excuteInPipe.inst(7) ## io.excuteInPipe.inst(30 downto 25) ## io.excuteInPipe.inst(11 downto 8) ## B"0",
    OP2.IMM_U -> io.excuteInPipe.inst(31 downto 12) ## Repeat(B(0,1 bits),12),
    OP2.IMM_S -> Repeat(io.excuteInPipe.inst.msb,20) ## io.excuteInPipe.inst(31 downto 25) ## io.excuteInPipe.inst(11 downto 7),
    OP2.IMM_J -> Repeat(io.excuteInPipe.inst.msb, 12) ## io.excuteInPipe.inst(19 downto 12) ## io.excuteInPipe.inst(20) ## io.excuteInPipe.inst(30 downto 21) ## B"0",
    default -> B(0,Xlen bits)
  )

  val ismuldiv = isMulDiv(io.opcode)
  val hold = UInt(HoldWidth bits)

  val alu = new Area{
    /* deal with the alu options */
    val aluPlugin = new ALUPlugin()
    aluPlugin.io.valid := io.opcode.illegal && io.excuteInPipe.valid
    aluPlugin.io.alu := io.opcode.alu
    aluPlugin.io.op1 := op1
    aluPlugin.io.op2 := op2
  }

  val muldiv = new Area{
    val muldivPlugin = new SimpleMulDivPlugin()
    muldivPlugin.io.valid := io.opcode.illegal && io.excuteInPipe.valid
    muldivPlugin.io.alu := io.opcode.alu
    muldivPlugin.io.op1 := op1
    muldivPlugin.io.op2 := op2
  }

  /* the lsu unit should be consider */
  val lsu = new Area{
    /* block lsu with one cycle latency */
    val memoryOp = io.opcode.illegal && io.excuteInPipe.valid && io.opcode.memoryOption =/= MemoryOp.NOT
    val readIt = RegInit(False).setWhen(io.opcode.illegal && io.excuteInPipe.valid && (io.opcode.memoryOption === MemoryOp.LOAD || io.opcode.memoryOption === MemoryOp.LOAD_U))
    val writeIt = RegInit(False).setWhen(io.opcode.illegal && io.excuteInPipe.valid && io.opcode.memoryOption === MemoryOp.STORE) /* fetch the memory happens */
    val mask = RegNextWhen(io.opcode.mask,memoryOp).init(Mask.WORD)
    val extend = RegNextWhen(io.opcode.memoryOption,memoryOp).init(MemoryOp.NOT)
    val writeReg = RegNextWhen(io.regs.reg_waddr,memoryOp).init(0)
    val address = RegNextWhen(alu.aluPlugin.io.res.asUInt,memoryOp).init(0)
    val writeValue = io.opcode.mask.mux(
      Mask.WORD -> io.regs.reg2_rdata_o,
      Mask.HALF -> io.regs.reg2_rdata_o(15 downto 0).resized,
      Mask.BYTE -> io.regs.reg2_rdata_o(7 downto 0).resized
    )
    val writeData = RegNextWhen(writeValue,memoryOp).init(0)
    val memoryCmd = MemoryCmd()
    memoryCmd.valid := readIt || writeIt
    memoryCmd.mask := mask.asBits
    memoryCmd.data := writeData
    memoryCmd.write := writeIt
    memoryCmd.address := address

    val splitIt = SplitBus()
    splitIt.io.memoryCmd <> memoryCmd
    val OnMemory = RegInit(False).setWhen(memoryOp).clearWhen(splitIt.io.dBus.read.rsp.fire || splitIt.io.dBus.write.rsp.fire || splitIt.io.peripheralBus.cmd.fire)

    when(memoryOp || OnMemory){
      hold := Hold_Decode /* hold all unit */
    }.otherwise{
      hold := 0 /* release it */
    }
    io.holdEx := hold
    writeIt.clearWhen(splitIt.io.dBus.write.cmd.fire || splitIt.io.peripheralBus.cmd.fire)
    readIt.clearWhen(splitIt.io.dBus.read.cmd.fire || splitIt.io.peripheralBus.cmd.fire)

    io.dBus <> splitIt.io.dBus
    io.peripheralBus <> splitIt.io.peripheralBus
  }


  val csr = new Area{
    val isCsr = io.excuteInPipe.valid && io.opcode.illegal && io.opcode.csr =/= CSR.N /* show about the csr value */
  }

  val jump = new Area{

  }

  val writeBack = new Area{
    /* control the write Back unit */
    val lsuWriteBack = lsu.OnMemory
    val lsuWriteIt = lsu.splitIt.io.dBus.read.rsp.fire || lsu.splitIt.io.peripheralBus.rsp.fire
    /* think about it*/
    val memory = lsuWriteBack || lsu.memoryOp
    val Unsigned = lsu.extend === MemoryOp.LOAD_U
    val dbusRsp = lsu.mask.mux(
      Mask.WORD -> lsu.splitIt.io.dBus.read.rsp.data,
      Mask.HALF -> Mux(Unsigned,lsu.splitIt.io.dBus.read.rsp.data(15 downto 0).resize(Xlen),Repeat(lsu.splitIt.io.dBus.read.rsp.data.msb,16) ## lsu.splitIt.io.dBus.read.rsp.data(15 downto 0)),
      Mask.BYTE -> Mux(Unsigned,lsu.splitIt.io.dBus.read.rsp.data(7 downto 0).resize(Xlen),Repeat(lsu.splitIt.io.dBus.read.rsp.data.msb,24) ## lsu.splitIt.io.dBus.read.rsp.data(7 downto 0))
    )
    val peripheralRsp = lsu.mask.mux(
      Mask.WORD -> lsu.splitIt.io.peripheralBus.rsp.data,
      Mask.HALF -> Mux(Unsigned,lsu.splitIt.io.peripheralBus.rsp.data(15 downto 0).resize(Xlen),Repeat(lsu.splitIt.io.peripheralBus.rsp.data.msb,16) ## lsu.splitIt.io.peripheralBus.rsp.data(15 downto 0)),
      Mask.BYTE -> Mux(Unsigned,lsu.splitIt.io.peripheralBus.rsp.data(7 downto 0).resize(Xlen),Repeat(lsu.splitIt.io.peripheralBus.rsp.data.msb,24) ## lsu.splitIt.io.peripheralBus.rsp.data(7 downto 0))
    )
    val rsp = Mux(lsu.splitIt.io.dBus.read.rsp.fire,dbusRsp,peripheralRsp)

    io.rfwrite.we := Mux(memory,lsuWriteIt,io.regs.reg_we)
    io.rfwrite.waddr := Mux(memory,lsu.writeReg,io.regs.reg_waddr)
    io.rfwrite.wdata := Mux(memory,rsp,Mux(ismuldiv,muldiv.muldivPlugin.io.res,alu.aluPlugin.io.res))
  }
}

object Excute extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Excute())
}