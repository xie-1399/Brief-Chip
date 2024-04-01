package TinyCore.Soc

/* the soc with the cpu kernel */
import TinyCore.Core.Kernel
import spinal.core._
import spinal.lib._
import spinal.core.sim._
import spinal.lib.bus.amba3.apb.Apb3Decoder
import spinal.lib.com.uart._
import spinal.lib.bus.amba4.axi._
import TinyCore.Peripheral._
import TinyCore.Core.Constant.Parameters._

object CoreConfig{
  /* more cpu config is set here */
  def gpioWidth = 2
  def peripheralAddr = 20

  def uartCtrlConfig = UartCtrlMemoryMappedConfig(
    uartCtrlConfig = UartCtrlGenerics(
      dataWidthMax = 8,
      clockDividerWidth = 20,
      preSamplingSize = 1,
      samplingSize = 5,
      postSamplingSize = 2
    ),
    txFifoDepth = 16,
    rxFifoDepth = 16
  )
}

class CPU extends Component{
  import CoreConfig._

  val io = new Bundle {
    val clk = in Bool()
    val reset = in Bool()
    val jtagReset = in Bool()
    val axi4 = master(Axi4(kernelAxi4Config))
    val gpi = in Bits (gpioWidth bits)
    val gpo = out Bits (gpioWidth bits)
    val uart = master (Uart())
  }

    val AsyncResetClockDomain = ClockDomain(
      clock = io.clk,
      config = ClockDomainConfig(resetKind = BOOT,resetActiveLevel = LOW)
    )
    val resetCtrl = new ClockingArea(AsyncResetClockDomain) {
      val AsyncReset = RegNext(io.reset) simPublic()
    }

    val systemClockDomain = ClockDomain(
      clock = io.clk,
      reset = resetCtrl.AsyncReset,
      frequency = FixedFrequency(100 MHz)
    )

  val Soc = new ClockingArea(systemClockDomain){
    val kernel = new Kernel(true) /* for debug use */
    val apb3Gpio = new Apb3Gpio(addrWidth = peripheralAddr,gpioWidth = gpioWidth)
    val apb3Clint = new Apb3Clint(addrWidth = peripheralAddr)
    val apb3Ram = new Apb3Ram(addrWidth = peripheralAddr)
    val apb3Rom = new Apb3Rom(addrWidth = peripheralAddr)
    val apb3Uart = Apb3UartCtrl(uartCtrlConfig)
    apb3Uart.io.apb.addAttribute(Verilator.public)
    apb3Uart.io.uart <> io.uart
    apb3Gpio.io.gpi := io.gpi
    io.gpo := apb3Gpio.io.gpo
    val apb3Decoder = Apb3Decoder(
      master = kernel.io.apb, /* the master apb addr width should > slave */
      slaves = List(
        apb3Uart.io.apb -> (0x00000,32 KiB),
        apb3Rom.io.apb -> (0x10000,32 KiB),
        apb3Ram.io.apb -> (0x20000,32 KiB),
        apb3Gpio.io.apb -> (0x30000,1 KiB),
        apb3Clint.io.apb -> (0x40000,1 KiB)
      )
    )
    kernel.io.jtagReset := io.jtagReset
    kernel.io.axi4 >> io.axi4
  }
}

object CPU extends App{
  SpinalSystemVerilog(new CPU())
}