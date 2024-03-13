package SimTools
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.amba4.axi._
object untils {

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

  /* master init */
  def Axi4ReadOnlyInit(bus: Axi4ReadOnly, readOnly: Boolean) = {
    require(readOnly)
    val ar = bus.ar
    val r = bus.r
    ar.ready #= false
    r.valid #= false
    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0
  }

}
