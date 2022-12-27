package mrv

import chisel3._
import chisel3.util._

class Ctrl extends Bundle {
  val exception = Bool()
  val aluOp = AluOp()
  val rs1 = UInt(4.W)
  val rs2 = UInt(4.W)
  val rd = UInt(4.W)
  val imm = SInt(32.W)

  val useImm = Bool()
  val isJump = Bool()
  val isLoad = Bool()
  val isStore = Bool()
}

class Decoder extends Module {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val ctrl = Output(new Ctrl)
  })

  val signals = ListLookup(
    io.inst,
    //   halt,   format,   aluOp,      jump,    load
    List(true.B, DontCare, AluOp.NONE, false.B, true.B),
    Array(
      Inst.ADD -> List(false.B, InstFormat.R, AluOp.ADD, false.B, false.B),
      Inst.ADDI -> List(false.B, InstFormat.I, AluOp.ADD, false.B, false.B),
      Inst.SUB -> List(false.B, InstFormat.R, AluOp.SUB, false.B, false.B),
      Inst.AND -> List(false.B, InstFormat.R, AluOp.AND, false.B, false.B),
      Inst.JAL -> List(false.B, InstFormat.J, AluOp.NONE, true.B, false.B),
      Inst.LW -> List(false.B, InstFormat.I, AluOp.ADD, false.B, true.B),
      Inst.SW -> List(false.B, InstFormat.S, AluOp.ADD, false.B, false.B),
    ),
  )

  io.ctrl.exception := signals(0)
  val instFormat = signals(1)
  io.ctrl.aluOp := signals(2)
  io.ctrl.isJump := signals(3)
  io.ctrl.isLoad := signals(4)
  io.ctrl.isStore := false.B

  io.ctrl.rd := io.inst(11, 7)
  io.ctrl.rs1 := io.inst(19, 15)
  io.ctrl.rs2 := io.inst(24, 20)

  // Decode immediate
  io.ctrl.useImm := false.B
  io.ctrl.imm := DontCare
  switch(instFormat) {
    is(InstFormat.I) {
      io.ctrl.imm := io.inst(31, 20).asSInt()
      io.ctrl.useImm := true.B
    }
    is(InstFormat.J) {
      io.ctrl.imm := Cat(
        Fill(12, io.inst(31)),
        io.inst(19, 12),
        io.inst(20),
        io.inst(30, 25),
        io.inst(24, 21),
        0.U(1.W),
      ).asSInt()
      io.ctrl.useImm := true.B
    }
    is(InstFormat.S) {
      io.ctrl.imm := Cat(
        io.inst(31, 25),
        io.inst(11, 7),
      ).asSInt()
      io.ctrl.isStore := true.B
      io.ctrl.useImm := true.B
    }
  }
}
