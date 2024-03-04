package Common

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.collection.mutable._

object SimUntils {

  /* using to adapter the string width */
  def StringAdapter(str: String, width: Int, fill: String = "0", left: Boolean = true): String = {
    if (str.length < width) {
      if (left) {
        fill * (width - str.length) + str
      } else {
        str + fill * (width - str.length)
      }
    } else {
      str
    }
  }


  /* get the memory value into the array */
  def getMemValue[T <: Data](mem:Mem[T],depth:Long) = {
    val buffer = ArrayBuffer[BigInt]()
    for(idx <- 0l until depth){
      buffer += mem.getBigInt(idx)
    }
    buffer
  }
}
