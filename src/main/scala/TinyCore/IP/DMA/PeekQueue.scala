package TinyCore.IP.DMA

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.4.2
 * /* just like a simple stream fifo */
 * =======================================================
 */
import spinal.core._
import spinal.lib._


class PeekQueue[T <: Data](Elem:HardType[T],Depth:Int) extends Component {
  require(isPow2(Depth))
  val ADDR_WIDTH = log2Up(Depth)

  val io = new Bundle{
    val Enqueue = slave(Stream(Elem))
    val Dequeue = master(Stream(Elem))
  }
  noIoPrefix()

  val counter = Reg(UInt(ADDR_WIDTH + 1 bits)).init(0)
  val rdptr,wrptr = Reg(UInt(ADDR_WIDTH bits)).init(0)
  val mem = Mem(Elem,Depth)

  /* counter logic */
  when(io.Dequeue.fire && io.Enqueue.fire){
    counter := counter
  }.elsewhen(io.Dequeue.fire){
    counter := counter - 1
  }.elsewhen(io.Enqueue.fire){
    counter := counter + 1
  }

  mem.write(wrptr,io.Enqueue.payload,enable = io.Enqueue.fire)


  when(io.Enqueue.fire){wrptr := wrptr + 1}
  when(io.Enqueue.fire){rdptr := rdptr + 1}
  io.Enqueue.ready := counter =/= Depth
  io.Dequeue.valid := counter =/= 0
  io.Dequeue.payload := mem.readAsync(rdptr)
}