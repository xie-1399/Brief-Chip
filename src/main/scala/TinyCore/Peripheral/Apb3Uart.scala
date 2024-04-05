package TinyCore.Peripheral

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.28
 * the uart is the simple device for checking the out and input
 * =======================================================
 */

class Apb3Uart(addrWidth:Int = 32,dataWidth:Int = 32) extends Component {
  val io = new Bundle{
    val bus = slave(Apb3(addrWidth,dataWidth))
    val tx = out Bool()
    val rx = in Bool()
  }



  val UartReg = new Area{



  }


  val TX = new Area{

  }

  val RX = new Area{

  }


}
