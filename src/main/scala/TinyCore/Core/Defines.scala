package TinyCore.Core

import spinal.core._

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.25
 * the constant for the cpu config and the decode Inst type
 * =======================================================
 */


object Defines{

  /* cpu top */
  def CPUReset = 0x80000000l
  def Xlen = 32
  def JumpEnable: Bool = True
  def JumpDisable: Bool = False


  /* hold the pipeline defines */
  def HoldEnable: Bool = True
  def HoldDisable: Bool = False
  def HoldWidth = 3
  def Hold_None: UInt = U(0, HoldWidth bits)
  def Hold_PC: UInt = U(1, HoldWidth bits)
  def Hold_Fetch: UInt = U(2, HoldWidth bits)

  /* about Bus */
  def InstBusDataWidth = 32
  def InstBusAddrWidth = 32
  def MemBus = 32
  def MemAddrBus = 32
  def MasterNum = 4
  def SlaveNum = 5
  def slave_0 = U(0,4 bits)
  def slave_1 = U(1,4 bits)
  def slave_2 = U(2,4 bits)
  def slave_3 = U(3,4 bits)
  def slave_4 = U(4,4 bits)



  /* some constant value */
  def ZeroWord: Bits = B(0, InstBusDataWidth bits)


}


object Instruction{

  def INST_NOP: Bits = B(19, 32 bits) /* 0x00000013 (00010011) addi x0,x0,0 */

  /* define the default inst */
  def INST_DEFAULT:Bits = B(1,32 bits)
  def INST_DEFAULT_OP:Bits = B(1, 7 bits)

}
