package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object AluOp extends ChiselEnum {
  val NONE = Value
  val ADD  = Value
  val AND  = Value
  val EQ   = Value
  val GE   = Value
  val GEU  = Value
  val LT   = Value
  val LTU  = Value
  val NE   = Value
  val NEQ  = Value
  val OR   = Value
  val SLL  = Value
  val SLT  = Value
  val SLTU = Value
  val SRA  = Value
  val SRL  = Value
  val SUB  = Value
  val XOR  = Value
}

class Alu extends Module {
  val io = IO(new Bundle {
    val op   = Input(AluOp())
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val out  = Output(UInt(32.W))
  })

  io.out := 0.U
  switch(io.op) {
    is(AluOp.ADD) {
      io.out := io.src1 + io.src2
    }
    is(AluOp.SUB) {
      io.out := io.src1 - io.src2
    }
    is(AluOp.AND) {
      io.out := io.src1 & io.src2
    }
    is(AluOp.EQ) {
      io.out := io.src1 === io.src2
    }
    is(AluOp.NE) {
      io.out := io.src1 =/= io.src2
    }
    is(AluOp.LT) {
      io.out := io.src1.asSInt < io.src2.asSInt
    }
    is(AluOp.LTU) {
      io.out := io.src1 < io.src2
    }
    is(AluOp.GE) {
      io.out := io.src1.asSInt >= io.src2.asSInt
    }
    is(AluOp.GEU) {
      io.out := io.src1 >= io.src2
    }
    is(AluOp.SRA) {
      io.out := (io.src1.asSInt >> io.src2(4, 0)).asUInt
    }
    is(AluOp.SRL) {
      io.out := io.src1 >> io.src2(4, 0)
    }
    is(AluOp.SLL) {
      io.out := io.src1 << io.src2(4, 0)
    }
    is(AluOp.OR) {
      io.out := io.src1 | io.src2
    }
    is(AluOp.XOR) {
      io.out := io.src1 ^ io.src2
    }
  }
}
