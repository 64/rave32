package mrv

import chisel3._
import chisel3.util._

class CpuSignals extends Bundle {
  val halted = Bool()
}

class OneCycle extends Module {
  val io = IO(new Bundle {
    val mem = new MemoryPort
    val signals = Output(new CpuSignals)
  })

  val pc = RegInit(0.U(32.W))

  io.mem <> MemoryPort.default
  // io.mem.writeAddr.valid := false.B
  // io.mem.writeAddr.bits := DontCare
  // io.mem.writeData := DontCare

  val decoder = Module(new Decoder)

  io.mem.readAddr.valid := true.B
  io.mem.readAddr.bits := pc >> 2
  decoder.io.inst := io.mem.readData
  // printf("read instruction 0x%x while %d\n", decoder.io.inst, io.mem.readAddr.valid)

  val halted = RegNext(io.signals.halted, false.B)
  io.signals.halted := decoder.io.ctrl.exception || halted
  // when (!halted && decoder.io.ctrl.exception) {
  //   printf("decoder exception!\n")
  // }

  pc := pc + 4.U
}

class OneCycleSim(init: List[Int] = List(), numWords: Int = 8) extends Module {
  val io = IO(new Bundle {
    val signals = Output(new CpuSignals)
    val loaded = Output(Bool())
  })

  val mem = Module(new MemorySim(init, numWords))
  val core = Module(new OneCycle)

  io.signals <> core.io.signals
  io.loaded := mem.io.loaded

  mem.io.mem <> core.io.mem
  // mem.io.mem.readAddr := core.io.mem.readAddr
  // core.io.mem.readData := mem.io.mem.readData
  // mem.io.mem.writeAddr := core.io.mem.writeAddr
  // mem.io.mem.writeData := core.io.mem.writeData

  core.reset := !io.loaded
}
