package Common

import spinal.core._
import spinal.lib._
import spinal.core.sim._
import scala.collection.mutable._
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math._
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

  def HexStringWithWidth(hex: String, width: Int, fill: String = "0", left: Boolean = true): String = {
    if (hex.length < width) {
      if (left) {
        fill * (width - hex.length) + hex
      } else {
        hex + fill * (width - hex.length)
      }
    } else {
      hex
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

  def BinFile2BigInt(Bin:String,width:Int,count:Int) = {
    /* convert the binary file*/
    val sources = Files.readAllBytes(Paths.get(Bin))
    require(width % 8 == 0)
    val period = width / 8
    val RawBuffer = ArrayBuffer[BigInt]()
    val valueBuffer = ArrayBuffer[BigInt]()
    sources.foreach{
      source =>
        if(source < 0){
          RawBuffer += source + 256
        }else{
          RawBuffer += source}
    }
    RawBuffer.grouped(period).foreach{
      datas =>
        var value = ""
        for(data <- datas.reverse){
          value += HexStringWithWidth(data.toLong.toBinaryString,8)
        }
        valueBuffer += BigInt(value,2)
    }
    if(valueBuffer.length < count){
      val remain = count - valueBuffer.length
      for(idx <- 0 until remain){valueBuffer += BigInt(0)}
    }
    valueBuffer
  }
}