package TinyCore.Peripheral

import spinal.lib.bus.amba3.apb.Apb3Config

object PeripheralConfig {

  def getApb3Config() = {
    val config = Apb3Config(32,32)
    config
  }

}
