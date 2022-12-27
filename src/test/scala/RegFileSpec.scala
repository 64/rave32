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
      c.io.writeEnable.poke(true.B)
      c.io.rd.poke(2.U)
      c.io.writeData.poke(7.U)
      c.clock.step()

      c.io.rs1.poke(2.U)
      c.io.rs1Data.peekInt() shouldBe 7
    }
  }

  it should "read zeros from the zero register" in {
    test(new RegFile) { c =>
      c.io.writeEnable.poke(false.B)
      c.io.rs1.poke(0.U)
      c.io.rs1Data.peekInt() shouldBe 0

      c.io.writeEnable.poke(true.B)
      c.io.writeData.poke(7.U)
      c.io.rd.poke(0.U)
      c.clock.step()

      c.io.writeEnable.poke(false.B)
      c.io.rs1.poke(0.U)
      c.io.rs1Data.peekInt() shouldBe 0
    }
  }

  it should "read rs1 and rs2 at the same time" in {
    test(new RegFile) { c =>
      c.io.writeEnable.poke(true.B)
      c.io.writeData.poke(3.U)
      c.io.rd.poke(1.U)
      c.clock.step()

      c.io.writeEnable.poke(true.B)
      c.io.writeData.poke(7.U)
      c.io.rd.poke(2.U)
      c.clock.step()

      c.io.rs1.poke(2.U)
      c.io.rs1Data.peekInt() shouldBe 7

      c.io.rs2.poke(1.U)
      c.io.rs2Data.peekInt() shouldBe 3
    }
  }
}
