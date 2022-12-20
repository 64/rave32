package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object AluOp extends ChiselEnum {
  val NONE = Value
  val ADD = Value
  val AND = Value
  val EQ = Value
  val GE = Value
  val GEU = Value
  val LT = Value
  val LTU = Value
  val NEQ = Value
  val NOT = Value
  val OR = Value
  val SUB = Value
  val XOR = Value
}

class Alu extends Module {
  val io = IO(new Bundle {
    val op = Input(AluOp())
    val src0 = Input(UInt(32.W))
    val src1 = Input(UInt(32.W))
    val dst = Output(UInt(32.W))
  })

  io.dst := 0.U
  switch(io.op) {
    is(AluOp.ADD) {
      io.dst := io.src0 + io.src1
    }
    is(AluOp.SUB) {
      io.dst := io.src0 - io.src1
    }
    is(AluOp.AND) {
      io.dst := io.src0 & io.src1
    }
    is(AluOp.NOT) {
      io.dst := ~io.src0
    }
  }
}
