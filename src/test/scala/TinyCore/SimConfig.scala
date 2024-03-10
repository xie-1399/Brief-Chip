package TinyCore

import TinyCore.Sim._

object SimConfig {

  def axi4ReadonlysimConfig = AxiReadonlyMemorySimConfig(
    maxOutstandingReads = 8,
    readResponseDelay = 2,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )



}
