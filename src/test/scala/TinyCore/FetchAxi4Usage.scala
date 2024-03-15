package TinyCore

import Common.SIMCFG
import org.scalatest.funsuite.AnyFunSuite
import TinyCore.Core.Fetch._
import TinyCore.Sim.AxiReadonlyMemorysim
import spinal.core.sim._

import scala.collection.mutable.Queue
import scala.util.Random
import SimTools.Tools._
class FetchAxi4Usage extends AnyFunSuite{

  /* in the vcs simulation -> a good idea is using the load simulation result */
  test("fetch the memory data with pc value going check"){

    SIMCFG().compile{
      val dut = new FetchAxi4()
      dut
    }.doSimUntilVoid{
      dut =>
        dut.clockDomain.forkStimulus(10)
        def init() = {
          dut.io.axiBus.r.valid #= false
          dut.io.jtagReset #= false
          dut.io.jump #= false
          dut.io.hold #= 1 /* hold the pc control */
          dut.io.jumpAddr.randomize()
          dut.clockDomain.waitSampling(5)
        }
        init()

        val mem = AxiReadonlyMemorysim(dut.io.axiBus,dut.clockDomain,SimConfig.axi4ReadonlysimConfig)
        val bytes = Array.fill(1024){Random.nextInt(127).toByte}
        val queue = Queue[String]()
        val byteArray = bytes.map(b => HexStringWithWidth(b.toHexString,2)).grouped(4).toArray
        for(array <- byteArray){
          val inst = array(3) + array(2) + array(1) + array(0)
          queue.enqueue(inst)
        }
        mem.memory.writeArray(address = 0x80000000l,bytes)
        mem.start()
        var index = 0
        val Inorder = fork{
          /* hold signals happens occur */
          while (index < (1024 / 4)){
            dut.io.hold #= Random.nextInt(3)
              if(dut.io.fetchOutPipe.valid.toBoolean){
                assert(queue.dequeue() == HexStringWithWidth(dut.io.fetchOutPipe.inst.toLong.toHexString,8))
                assert(dut.io.fetchOutPipe.pc.toLong.toHexString === (0x80000000l + index * 4).toHexString )
                index += 1
              }
            dut.clockDomain.waitSampling()
          }
        }

        val jumpCheck = fork{
          Inorder.join()
          dut.io.hold #= 0
          dut.io.jtagReset #= true
          dut.clockDomain.waitSampling()
          /* check the reset pc jump */

          dut.io.jtagReset #= false
          dut.io.jump #= true
          dut.io.jumpAddr #= 0x80000040l
          dut.clockDomain.waitSampling()
          dut.io.jump #= false
          dut.io.jumpAddr.randomize()
          dut.clockDomain.waitSampling(128)
        }
        jumpCheck.join()
        simSuccess()
    }


  }

}
