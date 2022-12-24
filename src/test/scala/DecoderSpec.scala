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
      c.io.inst.poke(0.U)
      check(c, "mul x1, x2, x3", AluOp.NONE, 0, 0, 0, 0, true)
    }
  }

  it should "decode ADD" in {
    test(new Decoder) { c =>
      check(c, "add x1, x2, x3", AluOp.ADD, 1, 2, 3, 0, false)
    }
  }

  it should "decode ADDI" in {
    test(new Decoder) { c =>
      check(c, "addi x1, x2, 3", AluOp.ADD, 1, 2, 0, 3, false)
    }
  }

  it should "decode SUB" in {
    test(new Decoder) { c =>
      check(c, "sub x1, x2, x3", AluOp.SUB, 1, 2, 3, 0, false)
    }
  }

  it should "decode AND" in {
    test(new Decoder) { c =>
      check(c, "and x1, x2, x3", AluOp.AND, 1, 2, 3, 0, false)
    }
  }

  def assemble(in: String): UInt = {
    ("b" + RISCVAssembler.binOutput(in)).U
  }

  def check(
      c: Decoder,
      inst: String,
      aluOp: AluOp.Type,
      rd: Int,
      rs1: Int,
      rs2: Int,
      imm: Int,
      exception: Boolean
  ) = {
    c.io.inst.poke(assemble(inst))
    c.io.ctrl.exception.peekBoolean() should be(exception)
    c.io.ctrl.aluOp.peek() should be(aluOp)
    c.io.ctrl.rd.peekInt() should be(rd)
    c.io.ctrl.rs1.peekInt() should be(rs1)
    c.io.ctrl.rs2.peekInt() should be(rs2)
  }
}
