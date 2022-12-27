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
      val readAddr = Input(UInt(3.W))
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

  regFile.io.rs1 := decoder.io.ctrl.rs1
  val rs1Data = regFile.io.rs1Data

  regFile.io.rs2 := decoder.io.ctrl.rs2
  val rs2Data = regFile.io.rs2Data

  regFile.io.rd := decoder.io.ctrl.rd
  regFile.io.writeEnable := false.B
  regFile.io.writeData := DontCare

  // printf(
  //   "rs1: %d (%d), rs2: %d (%d), rd: %d, out: %d\n",
  //   decoder.io.ctrl.rs1,
  //   op1,
  //   decoder.io.ctrl.rs2,
  //   op2,
  //   decoder.io.ctrl.rd,
  //   aluResult,
  // )

  val nextPc = WireInit(0.U(32.W))
  when(decoder.io.ctrl.isJump) {
    nextPc := pc + decoder.io.ctrl.imm.asUInt()

    // Set link register
    regFile.io.writeEnable := true.B
    regFile.io.writeData := pc + 4.U
  }.otherwise {
    nextPc := pc + 4.U

    val op1 = rs1Data
    val op2 = Wire(UInt(32.W))
    when(decoder.io.ctrl.useImm) {
      op2 := decoder.io.ctrl.imm.asUInt()
    }.otherwise {
      op2 := rs2Data
    }

    alu.io.src1 := op1
    alu.io.src2 := op2
    alu.io.op := decoder.io.ctrl.aluOp
    val aluResult = alu.io.out

    when(decoder.io.ctrl.isStore) {
      // mem[rs1 + offset] = rs2
      io.dmem.writeAddr.valid := true.B
      io.dmem.writeAddr.bits := aluResult
      io.dmem.writeData := rs2Data
    }.elsewhen(decoder.io.ctrl.isLoad) {
      // rd = mem[rs1 + offset]
      io.dmem.readAddr.valid := true.B
      io.dmem.readAddr.bits := aluResult
      regFile.io.writeEnable := true.B
      regFile.io.writeData := io.dmem.readData
    }.otherwise {
      // rd = rs1 `aluOp` rs2
      regFile.io.writeEnable := true.B
      regFile.io.writeData := aluResult
    }
  }

  pc := nextPc
  test.pc := pc
}

class OneCycleSim(init: List[Int] = List()) extends Module {
  val signals = IO(Output(new CpuSignals))

  // val dmem = Module(new MemorySim(List(), 32))
  val dmem = IO(new MemoryPort)
  val imem = Module(new MemorySim(init))
  val core = Module(new OneCycle)

  val test = IO(new Bundle {
    val loaded = Output(Bool())
    val pc = Output(UInt(32.W))
    val regs = new Bundle {
      val readAddr = Input(UInt(3.W))
      val readData = Output(UInt(32.W))
    }
  })
  test.loaded := imem.io.loaded
  test.pc := core.test.pc
  test.regs <> core.test.regs

  signals <> core.io.signals
  imem.io.mem <> core.io.imem
  dmem <> core.io.dmem

  core.reset := !test.loaded
}
