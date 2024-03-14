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

  /* bus init with  */
  def Axi4Init(bus: Axi4) = {
    val axi = bus
    val ar = axi.ar
    val r = axi.r
    val aw = axi.aw
    val w = axi.w
    val b = axi.b
    ar.ready #= false
    aw.ready #= false
    w.ready #= false
    r.valid #= false

    r.data #= 0
    if (r.config.useId) r.id #= 0
    if (r.config.useResp) r.resp #= 0
    if (r.config.useLast) r.last #= false
    if (r.config.useRUser) r.user #= 0

    b.valid #= false
    if (b.config.useId) b.id #= 0
    if (b.config.useResp) b.resp #= 0
    if (b.config.useBUser) b.user #= 0
  }

}
