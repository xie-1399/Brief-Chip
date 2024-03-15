package TinyCore.Core.Excute

import Common.SpinalTools.PrefixComponent
import TinyCore.Core.Decode._
import spinal.core._
import spinal.lib._
import TinyCore.Core.Constant.Parameters._
import TinyCore.Core.Constant.Defines._
import TinyCore.Core.Decode._
import TinyCore.Core.Pipeline.pipeSignals
import spinal.core.sim._
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
    val axiBus = master(Axi4(memoryAxi4Config))
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
  val hold = Reg(UInt(HoldWidth bits)).init(0)
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
    val writeData = Reg(Bits(Xlen bits)).init(0)
    writeData := mask.mux(
      Mask.WORD -> op2,
      Mask.HALF -> op2(15 downto 0).resized,
      Mask.BYTE -> op2(7 downto 0).resized
    )

    val dbus = DataMemBus()
    dbus.read.cmd.valid := readIt
    dbus.read.cmd.address := alu.aluPlugin.io.res.asUInt
    dbus.read.rsp.ready := True

    dbus.write.cmd.valid := writeIt
    dbus.write.cmd.mask := io.opcode.mask.asBits
    dbus.write.cmd.payload.data := writeData
    dbus.write.cmd.payload.address := alu.aluPlugin.io.res.asUInt
    dbus.write.rsp.ready := True

    when(memoryOp){
      hold := Hold_Decode /* hold all unit */
    }.elsewhen(dbus.read.rsp.fire || dbus.write.rsp.fire){
      hold := 0 /* release it */
    }
    io.holdEx := hold

    writeIt.clearWhen(dbus.write.cmd.fire)
    readIt.clearWhen(dbus.read.cmd.fire)
    io.axiBus << dbus.toAxi4() /* think about to pipe it one cycle */
  }

  val csr = new Area{

  }

  /* the write enable should adapt the memory */
  val arbitration = new Area{
    io.rfwrite.we := io.regs.reg_we
    io.rfwrite.waddr := io.regs.reg_waddr
    io.rfwrite.wdata := Mux(ismuldiv,muldiv.muldivPlugin.io.res,alu.aluPlugin.io.res)
  }
}

object Excute extends App{
  SpinalVerilog(SpinalConfig().withoutEnumString())(new Excute())
}