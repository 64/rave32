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
    while (!c.io.loaded.peekBoolean()) {
      c.clock.step()
    }
    // print("asd\n")
    println("-- begin test " + c.io.signals.halted.peekBoolean())
    // c.clock.step()
  }

  def assemble(in: String): Int = {
    Integer.parseUnsignedInt(RISCVAssembler.binOutput(in), 2)
  }

  it should "halt on invalid instruction" in {
    test(new OneCycleSim(List(0))) { c =>
      waitLoaded(c)
      c.io.signals.halted.peekBoolean() shouldBe true
    }
  }

  it should "keep asserting halt" in {
    test(new OneCycleSim(List(0, assemble("add x1, x2, x3")))).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      waitLoaded(c)
      for (i <- 0 until 10) {
        c.io.signals.halted.peekBoolean() shouldBe true
        c.clock.step()
      }
    }
  }

  it should "not halt on valid instruction" in {
    test(new OneCycleSim(List(assemble("add x1, x2, x3")))) { c =>
      waitLoaded(c)
      c.io.signals.halted.peekBoolean() shouldBe false
    }
  }

  it should "advance pc correctly" in {
    test(new OneCycleSim(List(assemble("add x1, x2, x3"), assemble("sub x1, x2, x3")))).withAnnotations(Seq(WriteVcdAnnotation)) { c =>
      waitLoaded(c)
      c.io.signals.halted.peekBoolean() shouldBe false
      c.clock.step()
      c.io.signals.halted.peekBoolean() shouldBe false
      c.clock.step()
      c.io.signals.halted.peekBoolean() shouldBe true
      c.clock.step()
    }
  }
}
