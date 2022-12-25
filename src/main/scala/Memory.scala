package mrv

import chisel3._
import chisel3.util._

class MemoryPort extends Bundle {
  val readAddr = Output(Valid(UInt(32.W)))
  val readData = Input(UInt(32.W))

  val writeAddr = Output(Valid(UInt(32.W)))
  val writeData = Output(UInt(32.W))
}
  
object MemoryPort {
  def default: MemoryPort = {
    val wire = Wire(new MemoryPort)
    wire.writeData := DontCare
    wire.readAddr := DontCare
    wire.writeAddr := DontCare
    wire.readAddr.valid := false.B
    wire.writeAddr.valid := false.B
    wire
  }
}

class Memory(words: Int = 8)
    extends Module {
  val io = IO(Flipped(new MemoryPort))

  private val mem = Mem(words, UInt(32.W))

  io.readData := 0.U
  when(io.readAddr.valid) {
    io.readData := mem.read(io.readAddr.bits)
    // printf("read 0x%x from address 0x%x\n", io.readData, io.readAddr.bits << 2)
  }

  when(io.writeAddr.valid) {
    mem.write(io.writeAddr.bits, io.writeData)
    // printf("writing 0x%x to address 0x%x\n", io.writeData, io.writeAddr.bits << 2)
  }
}

class MemorySim(init: List[Int] = List(), numWords: Int = 8) extends Module {
  val io = IO(new Bundle {
    val loaded = Output(Bool())
    val mem = Flipped(new MemoryPort)
  })

  val mem = Module(new Memory(numWords))
  val initRom = VecInit(
    init.map(x => x.U).toSeq ++ Seq.fill(numWords - init.size)(0.U)
  )

  mem.io <> io.mem
  // mem.io.readAddr := io.mem.readAddr
  // io.mem.readData := mem.io.readData
  // mem.io.writeAddr := io.mem.writeAddr
  // mem.io.writeData := io.mem.writeData

  val addr = RegInit(0.U(32.W))
  when(addr < numWords.U) {
    io.loaded := false.B
    mem.io.readAddr.valid := false.B
    mem.io.writeAddr.valid := true.B
    mem.io.writeAddr.bits := addr
    mem.io.writeData := initRom(addr)
    addr := addr + 1.U
  }.otherwise {
    io.loaded := true.B
  }
}
