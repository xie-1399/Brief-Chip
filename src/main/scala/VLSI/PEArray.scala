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
import Common.SIMCFG

class PEArray extends Component {

  val io = new Bundle{
    val ifm = in Bits(Ifm_DataWidth * IfmRows bits)
    val weight = in Bits(Wgt_DataWidth * WgtNums bits)
    val odd_cnt = in Bool()
    val p_init = in Bool()
    val p_write_zero = in Bool()
    val p_valid_data = in Bool() /* show the input is valid or not */

    val ifm_read = in Bool()
    val wgt_read = in Bool()
  }

  val rows = Vec(Reg(SInt(Ifm_DataWidth bits)).init(0),IfmRows)
  rows := io.ifm.asSInt.subdivideIn(IfmRows slices)

  val wgts = Vec(Reg(SInt(Wgt_DataWidth bits)).init(0),WgtNums)
  wgts := io.weight.asSInt.subdivideIn(WgtNums slices)

  val PEs = Array.fill(PERow){   /* 4 * 5*/
    Array.fill(PECol){new PE}}
  val PEWire = Array.fill(PERow) {
    Array.fill(PECol) {SInt(PE_DataWidth bits)}}

  val ifmBufs = Array.fill(IfmRows){new IfmBuf}
  val ifmBufWire = Array.fill(IfmRows){Vec(SInt(Ifm_DataWidth bits),BufSize)}

  val wgtBufs = Array.fill(WgtNums){new WgtBuf}
  val wgtBufWire = Array.fill(WgtNums){Vec(SInt(Wgt_DataWidth bits),BufSize)}

  val psumBufs = Array.fill(PsumNums){new PsumBuf}
  val fifo_outWire = Array.fill(PsumNums){Flow(SInt(PE_DataWidth bits))}

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

  /* the PSum Buffer */
  psumBufs.zipWithIndex.foreach{
    p =>
      p._1.io.p_init := io.p_init
      p._1.io.p_write_zero := io.p_write_zero
      p._1.io.p_valid_data := io.p_valid_data
      p._1.io.pe_data(0) := PEWire(0)(p._2)
      p._1.io.pe_data(1) := PEWire(1)(p._2)
      p._1.io.pe_data(2) := PEWire(2)(p._2)
      p._1.io.pe_data(3) := PEWire(3)(p._2)
      p._1.io.odd_cnt := io.odd_cnt

      fifo_outWire(p._2) := p._1.io.fifo_out
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
  import spinal.core.sim._
  /* test the PE Array */
  SIMCFG().compile{
    val dut = new PEArray
    dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      simSuccess()
  }

}