package Untils

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

}
