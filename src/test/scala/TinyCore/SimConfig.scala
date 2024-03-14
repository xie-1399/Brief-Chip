package TinyCore

import TinyCore.Sim._
import spinal.lib.bus.amba4.axi.sim.AxiMemorySimConfig
object SimConfig {

  def axi4ReadonlysimConfig = AxiReadonlyMemorySimConfig(
    maxOutstandingReads = 8,
    readResponseDelay = 0,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )

  def axi4simConfig = AxiMemorySimConfig(
    maxOutstandingReads = 8,
    readResponseDelay = 3,
    interruptProbability = 0,
    interruptMaxDelay = 0,
    defaultBurstType = 1,
    useAlteraBehavior = false
  )



}
