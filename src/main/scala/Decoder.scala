package mrv

import chisel3._
import chisel3.util._

class Ctrl extends Bundle {
  val exception = Bool()
  val aluOp = AluOp()
  val rs1 = UInt(4.W)
  val rs2 = UInt(4.W)
  val rd = UInt(4.W)
  val imm = UInt(11.W)
}

class Decoder extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val ctrl = Output(new Ctrl)
  })

  val signals = ListLookup(io.inst,
    List(true.B, DontCare, AluOp.NONE),
    Array(
      Inst.ADD -> List(false.B, InstFormat.R, AluOp.ADD),
      Inst.ADDI -> List(false.B, InstFormat.I, AluOp.ADD),
      Inst.SUB -> List(false.B, InstFormat.R, AluOp.SUB),
      Inst.AND -> List(false.B, InstFormat.R, AluOp.AND),
    )
  )

  io.ctrl.exception := signals(0)
  val instFormat = signals(1)
  io.ctrl.aluOp := signals(2)

  io.ctrl.rd := 0.U
  io.ctrl.rs1 := 0.U
  io.ctrl.rs2 := 0.U
  io.ctrl.imm := 0.U
  switch(instFormat) {
    is(InstFormat.R) {
      io.ctrl.rd := io.inst(11, 7)
      io.ctrl.rs1 := io.inst(19, 15)
      io.ctrl.rs2 := io.inst(24, 20)
    }
    is(InstFormat.I) {
      io.ctrl.rd := io.inst(11, 7)
      io.ctrl.rs1 := io.inst(19, 15)
      io.ctrl.imm := io.inst(31, 20)
    }
  }
}
