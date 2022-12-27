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
    c.test.regs.readAddr.poke(reg.U)
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
        List(assemble("add x1, x2, x3"), assemble("sub x1, x2, x3"), 0),
      ),
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
      new OneCycleSim(List(assemble("add x3, x0, x0"), assemble("jal x1, -4"))),
    ) { c =>
      waitLoaded(c)
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.signals.halted.peekBoolean() shouldBe false
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
      peekReg(c, 1) shouldBe 8
    }
  }

  it should "handle writes to registers" in {
    test(
      new OneCycleSim(List(assemble("addi x1, x0, 5"))),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
    }
  }

  it should "handle reads from registers" in {
    test(
      new OneCycleSim(
        List(assemble("addi x1, x0, 5"), assemble("addi x2, x1, 3")),
      ),
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
      new OneCycleSim(
        List(assemble("addi x1, x0, 3"), assemble("addi x1, x1, 7")),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 3
      c.clock.step()
      peekReg(c, 1) shouldBe 10
    }
  }

  it should "handle writes to memory" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 5"),
          assemble("sw x1, 9(x0)"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 5
      c.dmem.writeAddr.valid.peekBoolean() shouldBe true
      c.dmem.writeAddr.bits.peekInt() shouldBe 9
      c.dmem.writeData.peekInt() shouldBe 5
    }
  }

  it should "handle reads from memory" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 5"),
          assemble("sw x1, 16(x0)"),
          assemble("lw x2, 11(x1)"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      c.clock.step()
      c.dmem.readAddr.valid.peekBoolean() shouldBe true
      c.dmem.readAddr.bits.peekInt() shouldBe 16
      c.dmem.readData.poke(5)
      c.clock.step()
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "handle sub-word reads from memory" in {
    test(
      new OneCycleSim(
        List(
          assemble("lb, x2, 8(x0)"),
          assemble("lb, x3, 9(x0)"),
          assemble("lh, x4, 10(x0)"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.dmem.readAddr.valid.peekBoolean() shouldBe true
      c.dmem.readAddr.bits.peekInt() shouldBe 8
      c.dmem.readData.poke(0x15)
      c.clock.step()
      peekReg(c, 2) shouldBe 0x15

      c.dmem.readAddr.valid.peekBoolean() shouldBe true
      c.dmem.readAddr.bits.peekInt() shouldBe 8
      c.dmem.readData.poke(0xab17)
      c.clock.step()
      peekReg(c, 3) shouldBe 4294967211L

      c.dmem.readAddr.valid.peekBoolean() shouldBe true
      c.dmem.readAddr.bits.peekInt() shouldBe 8
      c.dmem.readData.poke(0xcdef1234L)
      c.clock.step()
      peekReg(c, 4) shouldBe 4294954479L
    }
  }
}
