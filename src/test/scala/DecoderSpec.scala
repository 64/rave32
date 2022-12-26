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
      checkJType(c, "jal x1, 124", AluOp.NONE, 1, 124)
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
      rs2: Int
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.rs2.peekInt() shouldBe rs2
    c.io.ctrl.useImm.peekBoolean() shouldBe false
  }

  def checkIType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      imm: Int
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.rs1.peekInt() shouldBe rs1
    c.io.ctrl.imm.peekInt() shouldBe imm
    c.io.ctrl.useImm.peekBoolean() shouldBe true
  }

  def checkJType(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      imm: Int
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() shouldBe false
    c.io.ctrl.jump.peekBoolean() shouldBe true
    c.io.ctrl.aluOp.peek() shouldBe aluOp
    c.io.ctrl.rd.peekInt() shouldBe rd
    c.io.ctrl.imm.peekInt() shouldBe imm
    c.io.ctrl.useImm.peekBoolean() shouldBe true
  }
}
