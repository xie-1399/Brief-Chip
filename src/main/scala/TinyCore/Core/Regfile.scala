package TinyCore.Core

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.21
 * RISC-V reg file get 32 regs and read and write about it seems ready
 * =======================================================
 */

import Common.SpinalTools.PrefixComponent
import spinal.core._
import spinal.lib._

class Regfile extends PrefixComponent{
  // jtag and decode stage will send the reg file cmd
  val io = new Bundle{


  }


}
