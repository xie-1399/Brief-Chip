package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * the partial sum buffer
 * =======================================================
 */

import spinal.core._
import spinal.lib._

import Constant._

/* seems the psum is like cal -> then to fifo */

class PsumBuf extends Component {

  val io = new Bundle{
    val pe_data = in Vec(SInt(PE_DataWidth bits),4)
    val p_write_zero = in Bool()
    val p_valid_data = in Bool()
    val p_init = in Bool()
    val odd_cnt = in Bool()

    val fifo_out = master Flow SInt(PE_DataWidth bits)
  }

  def Relu(sel:Bool,temp: SInt) = {
    val res = Mux(sel,S(0,temp.getWidth bits),temp)
    res
  }

  val d_odd_cnt = RegNext(io.odd_cnt).init(False) /* delay one cycle */

  val fifo1_in = Reg(Bits(PE_DataWidth bits)).init(0)
  val fifo2_in = Reg(Bits(PE_DataWidth bits)).init(0)
  val fifo1_rd_en = Reg(Bool()).init(False)
  val fifo2_rd_en = Reg(Bool()).init(False)
  val fifo1_wr_en = Reg(Bool()).init(False)
  val fifo2_wr_en = Reg(Bool()).init(False)
  val fifo1_empty = Bool()
  val fifo1_full = Bool()
  val fifo2_empty = Bool()
  val fifo2_full = Bool()
  val adderTree_out = SInt(PE_DataWidth bits)

  val p_valid = Reg(Bits(3 bits)).init(0) /* whether the input is valid */
  p_valid := p_valid(1 downto 0) ## io.p_valid_data
  val p_write_zero_reg = Reg(Bool()).init(False) // Whether the current write zero is valid data
  p_write_zero_reg := io.p_write_zero
  val write_zero = p_write_zero_reg || io.p_write_zero

  /* four stages pipeline as shifter */
  val fifo_out_i = Reg(Bits(PE_DataWidth bits)).init(0)
  val fifo_out_a = Reg(Bits(PE_DataWidth bits)).init(0)
  val fifo_out_i0 = Bits(PE_DataWidth bits)
  val fifo_out_i1 = Bits(PE_DataWidth bits)

  when(!d_odd_cnt){
    fifo_out_i := fifo_out_i1
    fifo_out_a := fifo_out_i0
  }.otherwise{
    fifo_out_i := fifo_out_i0
    fifo_out_a := fifo_out_i1
  }


  when(io.p_init){
    /* write is happening */
    fifo1_in := 0
    fifo1_rd_en := False
    fifo1_wr_en := True

    fifo2_in := 0
    fifo2_rd_en := False
    fifo2_wr_en := True
  }.elsewhen(write_zero){
    when(d_odd_cnt){
      fifo1_in := 0
      fifo1_wr_en := p_write_zero_reg
      fifo1_rd_en := io.p_write_zero

      fifo2_in := adderTree_out.asBits
      fifo2_wr_en := p_valid(2)
      fifo2_rd_en := p_valid(0)
    }.otherwise{
      fifo2_in := 0
      fifo2_wr_en := p_write_zero_reg
      fifo2_rd_en := io.p_write_zero

      fifo1_in := adderTree_out.asBits
      fifo1_wr_en := p_valid(2)
      fifo1_rd_en := p_valid(0)
    }
  }.elsewhen(!write_zero){
    when(d_odd_cnt){
      fifo1_in := 0
      fifo1_rd_en := False
      fifo1_wr_en := False

      fifo2_in := adderTree_out.asBits
      fifo2_wr_en := p_valid(2)
      fifo2_rd_en := p_valid(0)
    }.otherwise{
      fifo2_in := 0
      fifo2_rd_en := False
      fifo2_wr_en := False

      fifo1_in := adderTree_out.asBits
      fifo1_wr_en := p_valid(2)
      fifo1_rd_en := p_valid(0)
    }
  }.otherwise{
    fifo1_in := 0
    fifo1_rd_en := False
    fifo1_wr_en := False

    fifo2_in := 0
    fifo2_rd_en := False
    fifo2_wr_en := False
  }

  /* add tree to cal the total psum*/
  val adderTree = new PsumAdd
  adderTree.io.pe_data := io.pe_data
  adderTree.io.fifo_data := fifo_out_a.asSInt
  adderTree_out := adderTree.io.pe_psum

  /* using two fifo */
  val psumFifos = Array.fill(Psum_FIFONum){new SyncFIFOV2} /* fifo depth is 64 */
  psumFifos(0).io.rd_en := fifo1_rd_en
  psumFifos(0).io.wr_en := fifo1_wr_en
  fifo1_empty := psumFifos(0).io.empty
  fifo1_full := psumFifos(0).io.full
  psumFifos(0).io.dataIn := fifo1_in
  fifo_out_i0 := psumFifos(0).io.dataOut

  psumFifos(1).io.rd_en := fifo2_rd_en
  psumFifos(1).io.wr_en := fifo2_wr_en
  fifo2_empty := psumFifos(1).io.empty
  fifo2_full := psumFifos(1).io.full
  psumFifos(1).io.dataIn := fifo2_in
  fifo_out_i1 := psumFifos(1).io.dataOut
  /* io connect */
  io.fifo_out.valid := p_write_zero_reg
  io.fifo_out.payload := Relu(fifo_out_i.msb,fifo_out_i.asSInt)

}

object PsumBuf extends App{
  SpinalSystemVerilog(new PsumBuf)
}