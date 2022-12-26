package mrv

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

import chisel3._
import chiseltest._

class AluSpec extends AnyFlatSpec with ChiselScalatestTester {

  def checkAluOp(c: Alu, op: AluOp.Type, range: Range, model: (Long, Long) => Long) = {
    def reverse(x: Long): Long = scala.math.pow(2, 32).toLong - x
    def wrapNegatives(x: Long): Long = if (x < 0) { reverse(x.abs) } else { x }

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

  it should "add" in {
    test(new Alu) { c => checkAluOp(c, AluOp.ADD, -20 until 20, (x, y) => x + y) }
  }

  it should "subtract" in {
    test(new Alu) { c => checkAluOp(c, AluOp.SUB, -20 until 20, (x, y) => x - y) }
  }

  it should "and" in {
    test(new Alu) { c => checkAluOp(c, AluOp.AND, 0 until 40, (x, y) => x & y) }
  }

  it should "not" in {
    test(new Alu) { c => checkAluOp(c, AluOp.NOT, 0 until 40, (x, y) => ~x) }
  }
}
