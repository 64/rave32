package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import chisel3._
import chiseltest._

class RegFileSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "RegFile"

  it should "read back a written value" in {
    test(new RegFile) { c =>
      c.io.rd.valid.poke(true.B)
      c.io.rd.bits.poke(2.U)
      c.io.rdData.poke(7.U)
      c.clock.step()

      c.io.rs1.valid.poke(true.B)
      c.io.rs1.bits.poke(2.U)
      c.io.rs1Data.peekInt() shouldBe 7
    }
  }

  it should "read zeros from the zero register" in {
    test(new RegFile) { c =>
      c.io.rd.valid.poke(false.B)
      c.io.rs1.valid.poke(true.B)
      c.io.rs1.bits.poke(0.U)
      c.io.rs1Data.peekInt() shouldBe 0

      c.io.rd.valid.poke(true.B)
      c.io.rd.bits.poke(0.U)
      c.io.rdData.poke(7.U)
      c.clock.step()

      c.io.rd.valid.poke(false.B)
      c.io.rs1.valid.poke(true.B)
      c.io.rs1.bits.poke(0.U)
      c.io.rs1Data.peekInt() shouldBe 0
    }
  }

  it should "read rs1 and rs2 at the same time" in {
    test(new RegFile) { c =>
      c.io.rd.valid.poke(true.B)
      c.io.rd.bits.poke(1.U)
      c.io.rdData.poke(3.U)
      c.clock.step()

      c.io.rd.valid.poke(true.B)
      c.io.rd.bits.poke(2.U)
      c.io.rdData.poke(7.U)
      c.clock.step()

      c.io.rs1.valid.poke(true.B)
      c.io.rs1.bits.poke(2.U)
      c.io.rs1Data.peekInt() shouldBe 7

      c.io.rs2.valid.poke(true.B)
      c.io.rs2.bits.poke(1.U)
      c.io.rs2Data.peekInt() shouldBe 3
    }
  }
}
