package mrv

import chisel3._
import chisel3.util._

class RegFile extends Module {
  val io = IO(new Bundle {
    val rs1 = Input(Valid(UInt(3.W)))
    val rs2 = Input(Valid(UInt(3.W)))
    val rd = Input(Valid(UInt(3.W)))

    val rdData = Input(UInt(32.W))
    val rs1Data = Output(UInt(32.W))
    val rs2Data = Output(UInt(32.W))
  })

  val test = IO(new Bundle {
    val reg = Input(Valid(UInt(3.W)))
    val regData = Output(UInt(32.W))
  })

  val regs = Mem(16, UInt(32.W))
  io.rs1Data := 0.U
  io.rs2Data := 0.U
  test.regData := 0.U

  when(io.rs1.valid && io.rs1.bits =/= 0.U) {
    io.rs1Data := regs.read(io.rs1.bits)
    // printf("reading 0x%x from rs1=0x%x\n", io.rs1Data, io.rs1.bits)
  }

  when(io.rs2.valid && io.rs2.bits =/= 0.U) {
    io.rs2Data := regs.read(io.rs2.bits)
    // printf("reading 0x%x from rs2=0x%x\n", io.rs2Data, io.rs2.bits)
  }

  when(test.reg.valid && test.reg.bits =/= 0.U) {
    test.regData := regs.read(test.reg.bits)
  }

  when(io.rd.valid) {
    regs.write(io.rd.bits, io.rdData)
    // printf("writing 0x%x to rd=0x%x\n", io.rdData, io.rd.bits)
  }
}
