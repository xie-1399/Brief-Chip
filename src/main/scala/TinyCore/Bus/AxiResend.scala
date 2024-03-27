package TinyCore.Bus

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.3.24
 * if the axi bus time out will resend it
 * =======================================================
 */

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._

object getAxi4Config{
  val config = Axi4Config(addressWidth = 34, dataWidth = 64, idWidth = 2,
    useId = true,
    useRegion = false,
    useBurst = true,
    useLock = false,
    useCache = false,
    useSize = true,
    useQos = false,
    useLen = true,
    useLast = true,
    useResp = true,
    useProt = true,
    useStrb = true,
    arUserWidth = 3,
    awUserWidth = 3,
    rUserWidth = 3,
    wUserWidth = 3,
    bUserWidth = 3
  )

}


class AxiResend() extends Component{

  val io = new Bundle{
    val Prebus = slave (Axi4(getAxi4Config.config))
    val Aftbus = master (Axi4(getAxi4Config.config))
  }

  /* read cmd resent */
  val stageAr = RegNextWhen(io.Prebus.ar.payload,io.Prebus.ar.valid)

  val RCounter = Counter(1024 * 1024)





  /* write cmd resent */

}
