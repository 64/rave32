package mrv

import chisel3._
import chisel3.util._

class MemoryPort extends Bundle {
  val readAddr = Output(Valid(UInt(32.W)))
  val readData = Input(UInt(32.W))

  val writeAddr = Output(Valid(UInt(32.W)))
  val writeData = Output(UInt(32.W))
  val writeSize = Output(UInt(3.W)) // 1, 2 or 4
}

object MemoryPort {
  def default: MemoryPort = {
    val wire = Wire(new MemoryPort)
    wire.writeData := DontCare
    wire.readAddr := DontCare
    wire.writeAddr := DontCare
    wire.writeSize := DontCare
    wire.readAddr.valid := false.B
    wire.writeAddr.valid := false.B
    wire
  }
}

class Memory(words: Int = 8) extends Module {
  val io = IO(Flipped(new MemoryPort))

  private val mem = Mem(words, Vec(4, UInt(8.W)))

  io.readData := 0.U
  when(io.readAddr.valid) {
    val addr = io.readAddr.bits
    assert(addr(1, 0) === 0.U, "misaligned read address: %d", addr)
    val data = mem.read(addr >> 2)
    io.readData := Cat(data(3), data(2), data(1), data(0))
  }

  when(io.writeAddr.valid) {
    val addr = io.writeAddr.bits >> 2
    val off = io.writeAddr.bits(1, 0)
    val shifted = io.writeData << (off << 3)
    val split =
      VecInit(shifted(7, 0), shifted(15, 8), shifted(23, 16), shifted(31, 24))

    when(io.writeSize === 1.U) {
      val mask = UIntToOH(off)
      mem.write(addr, split, mask.asBools)
    }.elsewhen(io.writeSize === 2.U) {
      val mask = Mux(off(1), "b0011".U, "b1100".U)
      mem.write(addr, split, mask.asBools)
    }.elsewhen(io.writeSize === 4.U) {
      mem.write(addr, split)
    }.otherwise {
      assert(false.B, "invalid writeSize given: %d", io.writeSize)
    }
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
      mem.io.readAddr.valid := false.B
      mem.io.writeAddr.valid := true.B
      mem.io.writeAddr.bits := addr << 2
      mem.io.writeSize := 4.U
      mem.io.writeData := initRom(addr)
      addr := addr + 1.U
    }.otherwise {
      io.loaded := true.B
    }
  } else {
    io.loaded := true.B
  }
}
