package TinyCore.Core.Decode

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.11
 * evaluate the v1 and v2 decode on the FPGA
 * =======================================================
 */
import TinyCore.Core.Evaluate._
object DecodeReport extends App{

  val decodeV1 = evaluate("/mnt/eda1/Vivado/2021.2/bin","Decode.v",name = "DecodeV1")
  val decodeV2 = evaluate("/mnt/eda1/Vivado/2021.2/bin","DecodeV2.v",name = "DecodeV2")

}
