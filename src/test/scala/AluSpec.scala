package mrv

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chiseltest._

class AluSpec extends AnyFlatSpec with ChiselScalatestTester {

  def checkAluOp(
      c: Alu,
      op: AluOp.Type,
      range: Range,
      model: (Long, Long) => Long,
  ) = {
    def reverse(x: Long): Long = scala.math.pow(2, 32).toLong - x
    def wrapNegatives(x: Long): Long = if (x < 0) { reverse(x.abs) }
    else { x }

    for (i <- range) {
      for (j <- range) {
        c.io.op.poke(op)
        c.io.src1.poke(wrapNegatives(i).U)
        c.io.src2.poke(wrapNegatives(j).U)

        var out = wrapNegatives(model(i, j))
        c.io.out.expect(out.U, s"operands were $i and $j")
      }
    }
  }

  behavior of "ALU"

  it should "none" in {
    test(new Alu) { c => checkAluOp(c, AluOp.NONE, 0 until 40, (x, y) => 0) }
  }

  it should "add" in {
    test(new Alu) { c =>
      checkAluOp(c, AluOp.ADD, -20 until 20, (x, y) => x + y)
    }
  }

  it should "subtract" in {
    test(new Alu) { c =>
      checkAluOp(c, AluOp.SUB, -20 until 20, (x, y) => x - y)
    }
  }

  it should "and" in {
    test(new Alu) { c => checkAluOp(c, AluOp.AND, 0 until 40, (x, y) => x & y) }
  }

  it should "or" in {
    test(new Alu) { c => checkAluOp(c, AluOp.OR, 0 until 40, (x, y) => x | y) }
  }

  it should "xor" in {
    test(new Alu) { c => checkAluOp(c, AluOp.XOR, 0 until 40, (x, y) => x ^ y) }
  }

  // This is not entirely correct
  it should "sra" in {
    test(new Alu) { c => checkAluOp(c, AluOp.SRA, 0 until 20, (x, y) => x >> y) }
  }

  it should "srl" in {
    test(new Alu) { c => checkAluOp(c, AluOp.SRL, 0 until 20, (x, y) => x >> y) }
  }

  it should "sll" in {
    test(new Alu) { c => checkAluOp(c, AluOp.SLL, 0 until 20, (x, y) => x << y) }
  }

  it should "eq" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.EQ,
        0 until 40,
        (x, y) =>
          if (x == y) { 1 }
          else { 0 },
      )
    }
  }

  it should "ne" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.NE,
        0 until 40,
        (x, y) =>
          if (x != y) { 1 }
          else { 0 },
      )
    }
  }

  it should "lt" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.LT,
        -20 until 20,
        (x, y) =>
          if (x < y) { 1 }
          else { 0 },
      )
    }
  }

  it should "ltu" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.LTU,
        0 until 40,
        (x, y) =>
          if (x < y) { 1 }
          else { 0 },
      )
    }
  }

  it should "ge" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.GE,
        -20 until 20,
        (x, y) =>
          if (x >= y) { 1 }
          else { 0 },
      )
    }
  }

  it should "geu" in {
    test(new Alu) { c =>
      checkAluOp(
        c,
        AluOp.GEU,
        0 until 40,
        (x, y) =>
          if (x >= y) { 1 }
          else { 0 },
      )
    }
  }
}
