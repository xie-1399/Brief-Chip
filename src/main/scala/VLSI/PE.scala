package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * the PE unit is the really small unit to calculate mac
 * =======================================================
 */
import spinal.core._
import spinal.lib._
import Common.SIMCFG

import scala.util.Random

class PE(size:Int = 4) extends Component {
  require(size % 2 == 0)

  val io = new Bundle{
    val ifm = in(Vec(SInt(8 bits), size))
    val wgt = in(Vec(SInt(8 bits), size))
    val p_sum = out(SInt(25 bits))
  }
  noIoPrefix()

  val product = Vec(Reg(SInt(16 bits)).init(0),size)
  val p_sum = Reg(SInt(25 bits)).init(0)

  for(idx <- 0 until size){
    product(idx) := io.ifm(idx) * io.wgt(idx)
  }
  p_sum := product.reduceBalancedTree(_ +^ _).resized
  io.p_sum := p_sum
}

object PE extends App{
  import spinal.core.sim._
  SIMCFG(gtkFirst = true).compile{
   val dut = new PE(4)
   dut
  }.doSimUntilVoid{
    dut =>
      dut.clockDomain.forkStimulus(10)
      val randomSeed = 42 * Random.nextInt(10)
      for(idx <- 0 until randomSeed){
        val ifm = List.fill(4)(Random.nextInt(100) - 60)
        val wgt = List.fill(4)(Random.nextInt(100) - 60)
        val res = ifm.zipWithIndex.map(f => f._1 * wgt(f._2)).sum
        dut.io.ifm.zipWithIndex.foreach(f => f._1 #= ifm(f._2))
        dut.io.wgt.zipWithIndex.foreach(g => g._1 #= wgt(g._2))
        dut.clockDomain.waitSampling(3)
        assert(res == dut.io.p_sum.toBigInt,s"${res} not equal ${dut.io.p_sum.toBigInt}")
      }
      simSuccess()
  }
}
