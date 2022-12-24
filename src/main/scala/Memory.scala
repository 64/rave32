package mrv

import chisel3._
import chisel3.util._

class Memory(initialMemory: List[BigInt] = List(), words: Int = 8)
    extends Module {
  val io = IO(new Bundle {
    val readAddr = Input(Valid(UInt(32.W)))
    val readData = Output(UInt(32.W))

    val writeAddr = Input(Valid(UInt(32.W)))
    val writeData = Input(UInt(32.W))
  })

  val mem = Mem(words, UInt(32.W))

  private def doRead() = {
    io.readData := mem.read(io.readAddr.bits)
    // printf("read 0x%x from address 0x%x\n", io.readData, io.readAddr.bits)
  }

  private def doWrite() = {
    mem.write(io.writeAddr.bits, io.writeData)
    // printf("writing 0x%x to address 0x%x\n", io.writeData, io.writeAddr.bits)
  }

  io.readData := DontCare
  if (initialMemory.nonEmpty) {
    // Hack to pretend that the memory is initialised already.
    // TODO Replace by a proper loading mechanism in the testbench.
    var shadowState = Mem(words, Bool())
    var rom = VecInit(
      initialMemory.map(x => x.U).toSeq ++ Seq.fill(words - initialMemory.size)(
        0.U
      )
    )

    when(io.readAddr.valid) {
      when(shadowState.read(io.readAddr.bits)) {
        doRead()
      }.otherwise {
        io.readData := rom(io.readAddr.bits)
      }
    }

    when(io.writeAddr.valid) {
      doWrite()
      when(!shadowState.read(io.writeAddr.bits)) {
        shadowState.write(io.writeAddr.bits, true.B)
      }
    }

  } else {
    when(io.readAddr.valid) {
      doRead()
    }

    when(io.writeAddr.valid) {
      doWrite()
    }
  }
}
