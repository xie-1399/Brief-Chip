package TinyCore.Core

import TinyCore.Core.Decode.CtrlSignals
import TinyCore.Core.Constant.Defines._
import spinal.core._
import spinal.lib._
import spinal.lib.eda.bench._
import spinal.lib.eda.xilinx.VivadoFlow

object Pipeline{
  case class pipeSignals() extends Bundle with IMasterSlave {
    val pc = UInt(InstBusAddrWidth bits)
    val inst = Bits(InstBusDataWidth bits)
    val valid = Bool()

    override def asMaster(): Unit = {
      out(pc, inst, valid)
    }
  }
}

object Evaluate{

  /* test if 100 MHZ can work */

  def evaluate(vivadoPath:String,rtlPath:String,name:String = "top"): Unit = {
    // val vivadoPath = "/mnt/eda1/Vivado/2021.2"
    val workSpace = "./report"
    val family = "Artix 7"
    val device = "xc7a200tffv1156-3"
    val frequency = 100 MHz
    val cpu = 8

    val hist: Rtl = new Rtl {
    override def getName(): String = name
    override def getRtlPath(): String = rtlPath
    }

    val flow = VivadoFlow(vivadoPath,workSpace,hist,family,device,frequency,cpu)
    println(s"${family} ->${(flow.getFMax / 1e6).toInt} Mhz ${flow.getArea()}")
  }
}

object Untils {

  /* some useful hardware functions here */
  def assignBundleWithList[T <: Data](ctrls: CtrlSignals, seq: Seq[BaseType]) = {
    ctrls.flatten.zipWithIndex.map(ctrl => ctrl._1 := seq(ctrl._2))
  }

  def getOneNumber(thats: Seq[Bool]): UInt = { //Bitvector can convert to Bools
    CountOne(thats)
  }

  def equalWithList[T <: Data](data: T, content: Seq[T]): Bool = {
    val bools = content.map(_ === data)
    getOneNumber(bools) > 0
  }
}