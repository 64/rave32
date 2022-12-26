package mrv

import chisel3._
import chisel3.util._

class CpuSignals extends Bundle {
  val halted = Bool()
}

class OneCycle extends Module {
  val io = IO(new Bundle {
    val imem = new MemoryPort
    val dmem = new MemoryPort
    val signals = Output(new CpuSignals)
  })

  val test = IO(new Bundle {
    val pc = Output(UInt(32.W))
    val regs = new Bundle {
      val readAddr = Input(Valid(UInt(4.W)))
      val readData = Output(UInt(32.W))
    }
  })

  io.imem <> MemoryPort.default
  io.dmem <> MemoryPort.default

  val alu = Module(new Alu)
  alu.io.op := AluOp.NONE
  alu.io.src1 := DontCare
  alu.io.src2 := DontCare

  val regFile = Module(new RegFile)
  regFile.io.rs1.valid := false.B
  regFile.io.rs2.valid := false.B
  regFile.io.rd.valid := false.B
  regFile.io.rs1.bits := 5.U
  regFile.io.rs2.bits := DontCare
  regFile.io.rd.bits := DontCare
  regFile.io.rdData := DontCare

  regFile.test.reg := test.regs.readAddr
  test.regs.readData := regFile.test.regData

  val pc = RegInit(0.U(32.W))
  test.pc := pc
  io.imem.readAddr.valid := true.B
  io.imem.readAddr.bits := pc >> 2

  val decoder = Module(new Decoder)
  decoder.io.inst := io.imem.readData
  // printf("read instruction 0x%x while %d\n", decoder.io.inst, io.mem.readAddr.valid)

  val halted = RegNext(io.signals.halted, false.B)
  io.signals.halted := decoder.io.ctrl.exception || halted

  val nextPc = WireInit(0.U(32.W))
  when (decoder.io.ctrl.jump) {
    nextPc := pc + decoder.io.ctrl.imm.asUInt()
  }.otherwise {
    regFile.io.rs1.valid := true.B
    regFile.io.rs1.bits := decoder.io.ctrl.rs1
    val op1 = regFile.io.rs1Data

    val op2 = Wire(UInt(32.W))
    when (decoder.io.ctrl.useImm) {
      op2 := decoder.io.ctrl.imm.asUInt()
    }.otherwise {
      regFile.io.rs2.valid := true.B
      regFile.io.rs2.bits := decoder.io.ctrl.rs2
      op2 := regFile.io.rs2Data
    }

    alu.io.src1 := op1
    alu.io.src2 := op2
    alu.io.op := decoder.io.ctrl.aluOp
    val result = alu.io.out
    // printf("rs1: %d (%d), rs2: %d (%d), rd: %d, out: %d\n", decoder.io.ctrl.rs1, op1, decoder.io.ctrl.rs2, op2, decoder.io.ctrl.rd, result)

    regFile.io.rd.valid := true.B
    regFile.io.rd.bits := decoder.io.ctrl.rd
    regFile.io.rdData := result
    nextPc := pc + 4.U
  }


  pc := nextPc
  test.pc := pc
}

class OneCycleSim(init: List[Int] = List()) extends Module {
  val signals = IO(Output(new CpuSignals))

  val test = IO(new Bundle {
    val loaded = Output(Bool())
    val pc = Output(UInt(32.W))
    val regs = new Bundle {
      val readAddr = Input(Valid(UInt(4.W)))
      val readData = Output(UInt(32.W))
    }
  })

  val imem = Module(new MemorySim(init))
  val dmem = Module(new MemorySim(List(), 32))
  val core = Module(new OneCycle)

  signals <> core.io.signals
  test.loaded := imem.io.loaded
  test.pc := core.test.pc
  test.regs <> core.test.regs

  imem.io.mem <> core.io.imem
  dmem.io.mem <> core.io.dmem

  core.reset := !test.loaded
}
