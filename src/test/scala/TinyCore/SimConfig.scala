package TinyCore

import TinyCore.Sim._
import spinal.lib.bus.amba4.axi.sim.AxiMemorySimConfig
object SimConfig {

  def axi4ReadonlysimConfig = AxiReadonlyMemorySimConfig(
    maxOutstandingReads = 8,
    readResponseDelay = 0,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )

  def axi4simConfig = AxiMemorySimConfig(
    maxOutstandingReads = 8,
    readResponseDelay = 0,
    writeResponseDelay = 0,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )

  val testFile = Map[String,String](
    "ext/bench/rv32ui-p-add.bin" -> "800005b0",
          "ext/bench/rv32ui-p-addi.bin" -> "80000358",
          "ext/bench/rv32ui-p-and.bin" -> "80000588",
          "ext/bench/rv32ui-p-andi.bin" -> "80000290",
          "ext/bench/rv32ui-p-auipc.bin" -> "80000118",
          "ext/bench/rv32ui-p-beq.bin" -> "80000390",
          "ext/bench/rv32ui-p-bge.bin" -> "800003f0",
          "ext/bench/rv32ui-p-bgeu.bin" -> "80000424",
          "ext/bench/rv32ui-p-blt.bin" -> "80000390",
          "ext/bench/rv32ui-p-bltu.bin" -> "800003c4",
          "ext/bench/rv32ui-p-bne.bin" -> "80000394",
          "ext/bench/rv32ui-p-lui.bin" -> "80000130",
          "ext/bench/rv32ui-p-lw.bin" -> "8000035c",
          "ext/bench/rv32ui-p-or.bin" -> "80000594",
          "ext/bench/rv32ui-p-ori.bin" -> "800002ac",
          "ext/bench/rv32ui-p-jal.bin" -> "80000124",
          "ext/bench/rv32ui-p-jalr.bin" -> "800001a4",
          "ext/bench/rv32ui-p-slt.bin" -> "80000598",
          "ext/bench/rv32ui-p-slti.bin" -> "80000344",
          "ext/bench/rv32ui-p-sltiu.bin" -> "80000344",
          "ext/bench/rv32ui-p-sltu.bin" -> "80000598",
          "ext/bench/rv32ui-p-sub.bin" -> "80000590",
          "ext/bench/rv32ui-p-sw.bin" -> "80000554",
          "ext/bench/rv32ui-p-xor.bin" -> "80000590",
          "ext/bench/rv32ui-p-xori.bin" -> "800002b4",
          "ext/bench/rv32ui-p-simple.bin" -> "800000c0",
          "ext/bench/rv32um-p-mul.bin" -> "80000598",
          "ext/bench/rv32um-p-mulh.bin" -> "80000598",
          "ext/bench/rv32um-p-mulhu.bin" -> "80000598",
          "ext/bench/rv32um-p-div.bin" -> "800001ac",
          "ext/bench/rv32um-p-divu.bin" -> "800001b0",
          "ext/bench/rv32um-p-mulhsu.bin" -> "80000598",
          "ext/bench/rv32um-p-rem.bin" -> "800001ac",
          "ext/bench/rv32um-p-remu.bin" -> "800001ac",
          "ext/bench/rv32ui-p-lb.bin" -> "8000031c",
          "ext/bench/rv32ui-p-lbu.bin" -> "8000031c",
          "ext/bench/rv32ui-p-lh.bin" -> "8000033c",
          "ext/bench/rv32ui-p-lhu.bin" -> "80000350",
          "ext/bench/rv32ui-p-sb.bin" -> "800004c4",
          "ext/bench/rv32ui-p-sh.bin" -> "80000548",
          "ext/bench/rv32ui-p-sll.bin" -> "80000620",
          "ext/bench/rv32ui-p-slli.bin" -> "80000354",
          "ext/bench/rv32ui-p-sra.bin" -> "8000066c",
          "ext/bench/rv32ui-p-srai.bin" -> "80000388",
          "ext/bench/rv32ui-p-srl.bin" -> "80000654",
          "ext/bench/rv32ui-p-srli.bin" -> "80000370",
  )


}
