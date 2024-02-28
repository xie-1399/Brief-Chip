package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.25
 * Synchronous FIFO set here (bugs when writing and reading happened at the same time)
 * =======================================================
 */

import spinal.core._
import spinal.lib._
import Common.SIMCFG

import collection.mutable

class SyncFIFO(dataWidth:Int = 25,depth:Int = 64) extends Component {
  def addrWidth = log2Up(depth)

  val io = new Bundle{
    val dataIn = in Bits(dataWidth bits)
    val dataOut = out Bits(dataWidth bits)
    val wr_en = in Bool()  /* write enable*/
    val rd_en = in Bool()  /* read enable */
    val empty = out Bool()
    val full = out Bool()
  }
  val fifo = Mem(Bits(dataWidth bits),depth)
  val wr_ptr = Reg(UInt(addrWidth bits)).init(0)    /* write pointer */
  val rd_ptr = Reg(UInt(addrWidth bits)).init(0)    /* read pointer */
  val cnt = Reg(UInt(addrWidth + 1 bits)).init(0)
  val empty = cnt === 0
  val full = cnt === depth

  /* read */
  when(io.rd_en && !empty) {
    when(rd_ptr === depth - 1) {
      rd_ptr := 0
    }.otherwise {
      rd_ptr := rd_ptr + 1
    }
  }
  io.dataOut := fifo.readSync(rd_ptr,enable = io.rd_en && !empty)

  /* write */
  when(io.wr_en && !full){
    when(wr_ptr === depth - 1){
      wr_ptr := 0
    }.otherwise{
      wr_ptr := wr_ptr + 1
    }
  }
  fifo.write(wr_ptr,io.dataIn,io.wr_en && !full)

  /* cnt update */
  cnt := (io.wr_en ## io.rd_en).asUInt.mux(
    U(1,2 bits) -> Mux(!empty,cnt - 1 , cnt),
    U(2,2 bits) -> Mux(!full,cnt + 1 , cnt),
    default -> cnt
  )
  io.empty := empty
  io.full := full

}

/* the V2 seems checks ready for latency is 2 */

class SyncFIFOV2 extends Component{
  import Constant._
  val io = new Bundle{
    val dataIn = in Bits(PE_DataWidth bits)
    val dataOut = out Bits(PE_DataWidth bits)
    val wr_en = in Bool() /* write enable*/
    val rd_en = in Bool() /* read enable */
    val empty = out Bool()
    val full = out Bool()
  }

  val fifo = StreamFifo(Bits(PE_DataWidth bits),FIFODepth)

  val read = Stream(cloneOf(io.dataOut))
  val write = Stream(cloneOf(io.dataOut))
  read.ready := io.rd_en
  write.valid := io.wr_en

  write.payload := io.dataIn
  fifo.io.push.connectFrom(write)
  read.connectFrom(fifo.io.pop)

  /* using the Stream Fifo */
  io.dataOut := read.payload
  io.empty := fifo.io.occupancy === 0
  io.full := fifo.io.availability === 0
}

object SyncFIFOV2 extends App{
  import spinal.core.sim._
  SIMCFG().compile {
    val dut = new SyncFIFOV2()
    dut.fifo.io.simPublic()
    dut
  }.doSimUntilVoid {
    dut =>
      dut.clockDomain.forkStimulus(10)
      val queueModel = mutable.Queue[Long]()
      SimTimeout(1000000 * 10)
      dut.io.wr_en #= false
      dut.io.rd_en #= false
      dut.clockDomain.waitSampling()

      // Push data randomly, and fill the queueModel with pushed transactions.
      val pushThread = fork {
        dut.io.wr_en #= false
        while (true) {
          dut.io.wr_en.randomize()
          dut.io.dataIn.randomize()
          dut.clockDomain.waitSampling()
          if (dut.fifo.io.push.valid.toBoolean && dut.fifo.io.push.ready.toBoolean) {
            queueModel.enqueue(dut.fifo.io.push.payload.toLong)
          }
        }
      }

      // Pop data randomly, and check that it match with the queueModel.
      val popThread = fork {
        dut.io.rd_en #= true
        for (i <- 0 until 100000) {
          dut.io.rd_en.randomize()
          dut.clockDomain.waitSampling()
          if (dut.fifo.io.pop.valid.toBoolean && dut.fifo.io.pop.ready.toBoolean) {
            assert(dut.fifo.io.pop.payload.toLong == queueModel.dequeue())
          }
        }
        simSuccess()
      }
  }
}