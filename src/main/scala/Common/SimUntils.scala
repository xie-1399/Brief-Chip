package Common

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

}
