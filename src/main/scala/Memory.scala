package mrv

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object MemOp extends ChiselEnum {
  val NONE = Value
  val LB = Value
  val LH = Value
  val LW = Value
  val SB = Value
  val SH = Value
  val SW = Value
}

class MemoryPort extends Bundle {
  val addr = Output(UInt(32.W))
  val readData = Input(UInt(32.W))
  val writeData = Output(UInt(32.W))
  val memOp = Output(MemOp())
}

object MemoryPort {
  def default: MemoryPort = {
    val wire = Wire(new MemoryPort)
    wire.addr := DontCare
    wire.writeData := DontCare
    wire.memOp := MemOp.NONE
    wire
  }
}

class Memory(words: Int = 8) extends Module {
  val io = IO(Flipped(new MemoryPort))

  private val mem = Mem(words, Vec(4, UInt(8.W)))

  io.readData := 0.U // TODO
  when(io.memOp.isOneOf(Seq(MemOp.SB, MemOp.SH, MemOp.SW))) {
    val lookup = MuxLookup(
      io.memOp.asUInt,
      0.U,
      Array(
        MemOp.SB.asUInt -> "b0001".U,
        MemOp.SH.asUInt -> "b0011".U,
        MemOp.SW.asUInt -> "b1111".U,
      ).toIndexedSeq,
    )
    val off = io.addr(1, 0)
    val mask = lookup.rotateLeft(off)
    val shifted = MuxLookup(
      off,
      io.writeData.asTypeOf(Vec(4, UInt(8.W))),
      Seq(
        1.U -> VecInit(DontCare, io.writeData(7, 0), DontCare, DontCare),
        2.U -> VecInit(
          DontCare,
          DontCare,
          io.writeData(7, 0),
          io.writeData(15, 8),
        ),
        3.U -> VecInit(DontCare, DontCare, DontCare, io.writeData(7, 0)),
      ),
    )
    mem.write(
      io.addr >> 2,
      shifted.asTypeOf(Vec(4, UInt(8.W))),
      // shifted, // TODO: Why does this not work?
      mask.asBools,
    )
  }.elsewhen(io.memOp.isOneOf(Seq(MemOp.LB, MemOp.LH, MemOp.LW))) {
    // Read (or NONE)
    val rawData = mem.read(io.addr >> 2).asUInt
    val data = rawData >> (io.addr(1, 0) << 3)
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
    val mem = Flipped(new MemoryPort)
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
      io.loaded := false.B
      mem.io.addr := addr << 2
      mem.io.writeData := initRom(addr)
      mem.io.memOp := MemOp.SW
      addr := addr + 1.U
    }.otherwise {
      io.loaded := true.B
    }
  } else {
    io.loaded := true.B
  }
}
