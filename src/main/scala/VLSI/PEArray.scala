package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * the PE Array calculate the data with the RS dataflow
 * =======================================================
 */

import spinal.core._
import spinal.lib._
import Constant._


/* Todo know more about it and test it */

class PEArray extends Component {

  val io = new Bundle{
    val ifm = in Bits(64 bits)
    val weight = in Bits(32 bits)

    val ifm_read = in Bool()
    val wgt_read = in Bool()
  }

  val rows = Vec(Reg(SInt(Ifm_DataWidth bits)).init(0),8)
  rows := io.ifm.asSInt.subdivideIn(8 slices)

  val wgts = Vec(Reg(SInt(Wgt_DataWidth bits)).init(0),4)
  wgts := io.weight.asSInt.subdivideIn(4 slices)

  val PEs = Array.fill(4){   /* 4 * 5*/
    Array.fill(5){new PE}}
  val PEWire = Array.fill(4) {
    Array.fill(5) {SInt(PE_DataWidth bits)}}

  val ifmBufs = Array.fill(8){new IfmBuf}
  val ifmBufWire = Array.fill(8){Vec(SInt(Ifm_DataWidth bits),BufSize)}

  val wgtBufs = Array.fill(4){new WgtBuf}
  val wgtBufWire = Array.fill(4){Vec(SInt(Wgt_DataWidth bits),BufSize)}

  /* the weight and feature map buf connected */
  ifmBufs.zipWithIndex.foreach{
    f =>
      f._1.io.ifm_input := rows(f._2)
      f._1.io.ifm_read := io.ifm_read
      ifmBufWire(f._2) := f._1.io.ifm_bufout
  }

  wgtBufs.zipWithIndex.foreach {
    w =>
      w._1.io.wgt_input := wgts(w._2)
      w._1.io.wgt_read := io.wgt_read
      wgtBufWire(w._2) := w._1.io.wgt_bufout
  }


  /* PE connected */
  PEs.zipWithIndex.foreach{
    p =>
      p._1.zipWithIndex.foreach{
        e =>
          e._1.io.ifm := ifmBufWire(p._2 + e._2)
          e._1.io.wgt := wgtBufWire(p._2)
          PEWire(p._2)(e._2) := e._1.io.p_sum
      }
  }
}


object PEArray extends App{
  SpinalSystemVerilog(new PEArray)
}