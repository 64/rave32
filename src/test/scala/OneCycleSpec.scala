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
    val s = if (in == "ebreak") {
      "00000000000000000000000001110011"
    } else if (in == "ecall") {
      "00000000000100000000000001110011"
    } else if (in == "fence") {
      "00000000000000000000000000001111"
    } else {
      RISCVAssembler.binOutput(in)
    }
    Integer.parseUnsignedInt(s, 2)
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

  it should "handle jal" in {
    test(
      new OneCycleSim(
        List(
          assemble("add x0, x0, x0"),
          assemble("jal x1, -4"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
      peekReg(c, 1) shouldBe 8
    }
  }

  it should "handle jalr" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x3, x0, 8"),
          assemble("jalr x1, x3, 4"),
          assemble("addi x0, x0, 0"),
          assemble("addi x0, x0, 0"),
          assemble("addi x0, x0, 0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 12
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
      c.dmem.memOp.peek() shouldBe MemOp.SW
      c.dmem.addr.peekInt() shouldBe 9
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
      c.dmem.memOp.peek() shouldBe MemOp.LW
      c.dmem.addr.peekInt() shouldBe 16
      c.dmem.readData.poke(5)
      c.clock.step()
      peekReg(c, 2) shouldBe 5
    }
  }

  it should "handle auipc" in {
    test(
      new OneCycleSim(
        List(
          assemble("auipc x1, 0x1000"),
          assemble("auipc x2, 0x123000"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 0x1000
      c.clock.step()
      peekReg(c, 2) shouldBe 0x123004
    }
  }

  it should "handle lui" in {
    test(
      new OneCycleSim(
        List(
          assemble("lui x1, 0x1000"),
          assemble("lui x2, 0x123000"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.clock.step()
      peekReg(c, 1) shouldBe 0x1000
      c.clock.step()
      peekReg(c, 2) shouldBe 0x123000
    }
  }

  it should "handle backwards conditional branches" in {
    // Backward branches
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 1"),
          assemble("beq x1, x0, -4"),
          assemble("beq x1, x1, -8"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
    }
  }

  it should "handle forwards conditional branches" in {
    test(
      new OneCycleSim(
        List(
          assemble("addi x1, x0, 1"),
          assemble("blt x0, x1, 4"),
          assemble("blt x0, x1, 8"),
          assemble("add x0, x0, x0"),
          assemble("add x0, x0, x0"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 16
      c.clock.step()
      c.test.pc.peekInt() shouldBe 20
    }
  }

  it should "ignore ecall, ebreak and fence" in {
    test(
      new OneCycleSim(
        List(
          assemble("fence"),
          assemble("ecall"),
          assemble("ebreak"),
          assemble("add x0, x0, x0"),
        ),
      ),
    ) { c =>
      waitLoaded(c)
      c.test.pc.peekInt() shouldBe 0
      c.clock.step()
      c.test.pc.peekInt() shouldBe 4
      c.clock.step()
      c.test.pc.peekInt() shouldBe 8
      c.clock.step()
      c.test.pc.peekInt() shouldBe 12
    }
  }
}
