package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object MemOp extends ChiselEnum {
  val NONE = Value
  val LB   = Value
  val LH   = Value
  val LW   = Value
  val SB   = Value
  val SH   = Value
  val SW   = Value
}

class MemoryPort extends Bundle {
  val addr      = Output(UInt(32.W))
  val readData  = Input(UInt(32.W))
  val writeData = Output(UInt(32.W))
  val memOp     = Output(MemOp())
}

object MemoryPort {
  def default: MemoryPort = {
    val wire = Wire(new MemoryPort)
    wire.addr      := DontCare
    wire.writeData := DontCare
    wire.memOp     := MemOp.NONE
    wire
  }
}

class Memory(words: Int = 8) extends Module {
  val io = IO(Flipped(new MemoryPort))

  private val mem = Mem(words, Vec(4, UInt(8.W)))

  io.readData := DontCare
  when(io.memOp.isOneOf(Seq(MemOp.SB, MemOp.SH, MemOp.SW))) {
    val off  = io.addr(1, 0)
    val next = WireInit(mem.read(io.addr >> 2))
    switch(io.memOp) {
      is(MemOp.SB) {
        when(off === 0.U) {
          next(0) := io.writeData(7, 0)
        }.elsewhen(off === 1.U) {
          next(1) := io.writeData(7, 0)
        }.elsewhen(off === 2.U) {
          next(2) := io.writeData(7, 0)
        }.elsewhen(off === 3.U) {
          next(3) := io.writeData(7, 0)
        }
      }
      is(MemOp.SH) {
        when(off === 0.U) {
          next(0) := io.writeData(7, 0)
          next(1) := io.writeData(15, 8)
        }.elsewhen(off === 2.U) {
          next(2) := io.writeData(7, 0)
          next(3) := io.writeData(15, 8)
        }
      }
      is(MemOp.SW) {
        next(0) := io.writeData(7, 0)
        next(1) := io.writeData(15, 8)
        next(2) := io.writeData(23, 16)
        next(3) := io.writeData(31, 24)
      }
    }

    mem.write(
      io.addr >> 2,
      next,
    )
  }.elsewhen(io.memOp.isOneOf(Seq(MemOp.LB, MemOp.LH, MemOp.LW))) {
    // Read
    val rawData = mem.read(io.addr >> 2).asUInt
    val data    = rawData >> (io.addr(1, 0) << 3)
    io.readData := MuxLookup(
      io.memOp.asUInt,
      data,
      Seq(
        MemOp.LB.asUInt -> data(7, 0).asSInt.pad(32).asUInt,
        MemOp.LH.asUInt -> data(15, 0).asSInt.pad(32).asUInt,
      ),
    )
  }
}

class MemorySim(init: List[Int] = List(), var numWords: Int = 0)
    extends Module {
  val io = IO(new Bundle {
    val loaded = Output(Bool())
    val mem    = Flipped(new MemoryPort)
  })

  if (numWords == 0) {
    if (init.size == 0) {
      numWords = 8
    } else {
      numWords = init.size + 1
    }
  }

  val mem = Module(new Memory(numWords))
  mem.io <> io.mem

  if (init.size != 0) {
    val initRom = VecInit(init.map(x => x.S(32.W).asUInt()).toSeq ++ Seq(0.U))

    val addr = RegInit(0.U(32.W))
    when(addr < init.size.U) {
      io.loaded        := false.B
      mem.io.addr      := addr << 2
      mem.io.writeData := initRom(addr)
      mem.io.memOp     := MemOp.SW
      addr             := addr + 1.U
    }.otherwise {
      io.loaded := true.B
    }
  } else {
    io.loaded := true.B
  }
}
