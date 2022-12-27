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

  val regFile = Module(new RegFile)

  regFile.test.reg := test.regs.readAddr
  test.regs.readData := regFile.test.regData

  val pc = RegInit(0.U(32.W))
  test.pc := pc
  io.imem.addr := pc
  io.imem.memOp := MemOp.LW

  val decoder = Module(new Decoder)
  decoder.io.inst := io.imem.readData
  io.dmem.memOp := decoder.io.ctrl.memOp

  val alu = Module(new Alu)
  alu.io.op := decoder.io.ctrl.aluOp

  val halted = RegNext(io.signals.halted, false.B)
  io.signals.halted := decoder.io.ctrl.exception || halted

  regFile.io.rs1 := decoder.io.ctrl.rs1
  val rs1Data = regFile.io.rs1Data

  regFile.io.rs2 := decoder.io.ctrl.rs2
  val rs2Data = regFile.io.rs2Data

  regFile.io.rd := decoder.io.ctrl.rd
  regFile.io.writeEnable := false.B
  regFile.io.writeData := DontCare

  val nextPc = Wire(UInt(32.W))
  when(decoder.io.ctrl.isJump) {
    // Jump to pc + imm
    alu.io.src1 := pc
    alu.io.src2 := decoder.io.ctrl.imm.asUInt()
    nextPc := alu.io.out

    // Set link register
    regFile.io.writeEnable := true.B
    regFile.io.writeData := pc + 4.U
  }.otherwise {
    nextPc := pc + 4.U

    alu.io.src1 := rs1Data
    when(decoder.io.ctrl.useImm) {
      alu.io.src2 := decoder.io.ctrl.imm.asUInt()
    }.otherwise {
      alu.io.src2 := rs2Data
    }
    val aluResult = alu.io.out

    when(decoder.io.ctrl.memOp.isOneOf(Seq(MemOp.SB, MemOp.SH, MemOp.SW))) {
      // mem[rs1 + offset] = rs2
      io.dmem.addr := aluResult
      io.dmem.writeData := rs2Data
    }.elsewhen(decoder.io.ctrl.memOp.isOneOf(Seq(MemOp.LB, MemOp.LH, MemOp.LW))) {
      // rd = mem[rs1 + offset]
      io.dmem.addr := aluResult

      regFile.io.writeEnable := true.B
      regFile.io.writeData := io.dmem.readData
    }.otherwise {
      // rd = rs1 `aluOp` rs2
      regFile.io.writeEnable := true.B
      regFile.io.writeData := aluResult
    }
  }

  test.pc := pc

  when (!io.signals.halted) {
    pc := nextPc
  }
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
