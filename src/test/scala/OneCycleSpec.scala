package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import com.carlosedp.riscvassembler.RISCVAssembler

import chisel3._
import chiseltest._

class OneCycleSpec
    extends AnyFlatSpec
    with ChiselScalatestTester
    with Matchers {
  behavior of "OneCycle"

  def waitLoaded(c: OneCycleSim) = {
    while (!c.test.loaded.peekBoolean()) {
      c.clock.step()
    }
  }

  def assemble(in: String): Int = {
    Integer.parseUnsignedInt(RISCVAssembler.binOutput(in), 2)
  }
  
  def peekReg(c: OneCycleSim, reg: Int): BigInt = {
    c.test.regs.readAddr.valid.poke(true.B)
    c.test.regs.readAddr.bits.poke(reg.U)
    c.test.regs.readData.peekInt()
  }

  it should "halt on invalid instruction" in {
    test(new OneCycleSim(List(0))) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  it should "keep asserting halt" in {
    test(new OneCycleSim(List(0, assemble("add x1, x2, x3"))))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        waitLoaded(c)
        for (i <- 0 until 10) {
          c.signals.halted.peekBoolean() shouldBe true
          c.clock.step()
        }
      }
  }

  it should "not halt on valid instruction" in {
    test(new OneCycleSim(List(assemble("add x1, x2, x3")))) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
    }
  }

  it should "advance pc" in {
    test(
      new OneCycleSim(
        List(assemble("add x1, x2, x3"), assemble("sub x1, x2, x3"), 0)
      )
    ).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe true
    }
  }

  it should "handle jumps" in {
    test(
      new OneCycleSim(List(assemble("add x1, x2, x3"), assemble("jal x0, -4")))
    ) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
    }
  }

  it should "handle writes to registers" in {
    test(
      new OneCycleSim(List(assemble("addi x1, x0, 5")))
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
    }
  }

  it should "handle reads from registers" in {
    test(
      new OneCycleSim(List(assemble("addi x1, x0, 5"), assemble("addi x2, x1, 3")))
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
      c.clock.step()
      peekReg(c, 2) shouldBe 8
    }
  }

  it should "handle reads and writes to the same register" in {
    test(
      new OneCycleSim(List(assemble("addi x1, x0, 3"), assemble("addi x1, x1, 7")))
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 3
      c.clock.step()
      peekReg(c, 1) shouldBe 10
    }
  }
}
