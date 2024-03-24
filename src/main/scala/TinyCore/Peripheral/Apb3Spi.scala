package TinyCore.Peripheral

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.21
 * using the spi to trans with the data (simple version)
 * material: https://zhuanlan.zhihu.com/p/629727733
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

/* here is the spi master module */
class Apb3Spi(addrWidth:Int = 32, dataWidth:Int = 32) extends PrefixComponent {

  val io = new Bundle{
    val data_i = in Bits(dataWidth bits)
    val addr_i = in UInt(addrWidth bits)
    val we_i = in Bool()
    val data_o = out Bits(dataWidth bits)

    /* the spi signals */
    val spi_mosi = out Bool() /* the spi control output */
    val spi_miso = in Bool()  /* the spi control input */
    val spi_ss = out Bool()   /* select signal */
    val spi_clk = out Bool()   /* the device clock (half of the the clk)*/
  }

  def SPI_CTRL = 0x0  /* the control reg of the spi master */
  /* [0] - enable , [1] - CPOL , [2] - CPHA, [3]:ss , [15: 8] the clk div number */

  def SPI_DATA = 0x4  /* the spi cmd or inout data */
  def SPI_STATUS = 0x8 /* the spi status (show is busy or not) readonly */

  val spi_ctrl = Reg(Bits(dataWidth bits)).init(0)
  val spi_status = Reg(Bits(dataWidth bits)).init(0) /* 1: busy 0:idle */
  val spi_data = Reg(Bits(dataWidth bits)).init(0)
  val done = RegInit(False) /* the trans finish */
  val data_o = Reg(Bits(dataWidth bits)).init(0)  /* read the regs out */
  val addr = io.addr_i(3 downto 0)

  val enable = RegInit(False) /* the trans enable signal is valid when the trans is happening */
  when(spi_ctrl(0)){
    enable.set()
  }.elsewhen(done){
    enable.clear()
  }
  io.spi_ss := !spi_ctrl(3)
  val div_cnt = spi_ctrl(15 downto 8)
  val clk_cnt = Reg(UInt(9 bits)).init(0)

  when(enable){
    when(clk_cnt === div_cnt.asUInt){
      clk_cnt := 0
    }.otherwise{
      clk_cnt := clk_cnt + 1 // the internal clock count
    }
  }.otherwise{
    clk_cnt := 0
  }

  /* write and read about the regs internal */
  val regs = new Area {
    spi_status(0) := enable /* busy or not*/
    when(io.we_i){
      switch(addr){
        is(SPI_CTRL){spi_ctrl := io.data_i}
        is(SPI_DATA){spi_data := io.data_i}
      }
    }.otherwise{
      spi_ctrl(0) := False
      when(done){

      }
    }
    /* read the regs out */
    data_o := addr.mux(
      SPI_CTRL -> spi_ctrl,
      SPI_DATA -> spi_data,
      SPI_STATUS -> spi_status,
      default -> B(0,dataWidth bits)
    )
    io.data_o := data_o
  }

}
