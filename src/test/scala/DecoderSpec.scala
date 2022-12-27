package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import com.carlosedp.riscvassembler.RISCVAssembler

import chisel3._
import chiseltest._

class DecoderSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decoder"

  it should "not decode MUL" in {
    test(new Decoder) { c =>
      c.io.inst.poke(assemble("mul, x1, x2, x3"))
      c.io.ctrl.exception.peekBoolean() shouldBe true
    }
  }

  it should "decode ADD" in {
    test(new Decoder) { c =>
      checkRType(c, "add x1, x2, x3", AluOp.ADD, 1, 2, 3)
    }
  }

  it should "decode ADDI" in {
    test(new Decoder) { c =>
      checkIType(c, "addi x1, x2, 3", AluOp.ADD, 1, 2, 3)
    }
  }

  it should "decode SUB" in {
    test(new Decoder) { c =>
      checkRType(c, "sub x1, x2, x3", AluOp.SUB, 1, 2, 3)
    }
  }

  it should "decode AND" in {
    test(new Decoder) { c =>
      checkRType(c, "and x1, x2, x3", AluOp.AND, 1, 2, 3)
    }
  }

  it should "decode JAL" in {
    test(new Decoder) { c =>
      checkJType(c, "jal x1, 124", AluOp.ADD, 1, 124)
    }
  }

  it should "decode LB" in {
    test(new Decoder) { c =>
      checkIType(c, "lb x1, 124(x2)", AluOp.ADD, 1, 2, 124, 1)
    }
  }

  it should "decode LH" in {
    test(new Decoder) { c =>
      checkIType(c, "lh x1, 124(x2)", AluOp.ADD, 1, 2, 124, 2)
    }
  }

  it should "decode LW" in {
    test(new Decoder) { c =>
      checkIType(c, "lw x1, 124(x2)", AluOp.ADD, 1, 2, 124, 4)
    }
  }

  it should "decode SB" in {
    test(new Decoder) { c =>
      checkSType(c, "sb x1, 124(x2)", AluOp.ADD, 2, 1, 124, 1)
    }
  }

  it should "decode SH" in {
    test(new Decoder) { c =>
      checkSType(c, "sh x1, 124(x2)", AluOp.ADD, 2, 1, 124, 2)
    }
  }

  it should "decode SW" in {
    test(new Decoder) { c =>
      checkSType(c, "sw x1, 124(x2)", AluOp.ADD, 2, 1, 124, 4)
    }
  }

  def assemble(in: String): UInt = {
    ("b" + RISCVAssembler.binOutput(in)).U
  }

  def checkRType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      rs2: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isJump.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe false
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
  }

  def checkIType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      imm: Int,
      accessSize: Int = 0,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isJump.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.imm.peekInt() shouldBe imm

    if (accessSize == 4) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.LW
    } else if(accessSize == 2) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.LH
    } else if (accessSize == 1) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.LB
    }
  }

  def checkJType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      imm: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isJump.peekBoolean() shouldBe true
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.memOp.peek() shouldBe MemOp.NONE
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.imm.peekInt() shouldBe imm
  }

  def checkSType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rs1: Int,
      rs2: Int,
      offset: Int,
      accessSize: Int,
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.isJump.peekBoolean() shouldBe false
    c.io.ctrl.useImm.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
    c.io.ctrl.imm.peekInt() shouldBe offset

    if (accessSize == 4) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.SW
    } else if(accessSize == 2) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.SH
    } else if (accessSize == 1) {
      c.io.ctrl.memOp.peek() shouldBe MemOp.SB
    }
  }
}
