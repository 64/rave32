package mrv

import chisel3._
import chisel3.util._

class RegFile extends Module {
  val io = IO(new Bundle {
    val rs1 = Input(UInt(3.W))
    val rs2 = Input(UInt(3.W))
    val rd  = Input(UInt(3.W))

    val writeEnable = Input(Bool())
    val writeData   = Input(UInt(32.W))

    val rs1Data = Output(UInt(32.W))
    val rs2Data = Output(UInt(32.W))
  })

  val test = IO(new Bundle {
    val reg     = Input(UInt(3.W))
    val regData = Output(UInt(32.W))
  })

  val regs = Mem(16, UInt(32.W))
  io.rs1Data   := 0.U
  io.rs2Data   := 0.U
  test.regData := 0.U

  when(io.rs1 =/= 0.U) {
    io.rs1Data := regs.read(io.rs1)
    // printf("reading 0x%x from rs1=0x%x\n", io.rs1Data, io.rs1.bits)
  }

  when(io.rs2 =/= 0.U) {
    io.rs2Data := regs.read(io.rs2)
    // printf("reading 0x%x from rs2=0x%x\n", io.rs2Data, io.rs2.bits)
  }

  when(test.reg =/= 0.U) {
    test.regData := regs.read(test.reg)
  }

  when(io.writeEnable) {
    regs.write(io.rd, io.writeData)
    // printf("writing 0x%x to rd=0x%x\n", io.rdData, io.rd.bits)
  }
}
