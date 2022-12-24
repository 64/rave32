package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import chisel3._
import chiseltest._
import chisel3.util._

class MemorySpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory"

  def pokeRead(c: Memory, addr: Int): BigInt = {
    c.io.readAddr.valid.poke(true.B)
    c.io.readAddr.bits.poke(addr.U)
    c.io.readData.peekInt()
  }

  def pokeWrite(c: Memory, addr: Int, value: Int) = {
    c.io.writeAddr.valid.poke(true.B)
    c.io.writeAddr.bits.poke(addr.U)
    c.io.writeData.poke(value.U)
  }

  it should "read zeros after reset" in {
    test(new Memory) { c =>
      c.io.writeAddr.initSource().setSourceClock(c.clock)
      c.io.readAddr.initSource().setSourceClock(c.clock)

      for (i <- 0 until 20) {
        pokeRead(c, i) shouldBe 0
      }
    }
  }

  it should "allow initialisation from a file" in {
    test(new Memory(List(0x123, 0x456))) { c =>
      c.io.writeAddr.initSource().setSourceClock(c.clock)
      c.io.readAddr.initSource().setSourceClock(c.clock)

      pokeRead(c, 0) shouldBe 0x123
      pokeRead(c, 1) shouldBe 0x456
      pokeRead(c, 2) shouldBe 0
    }
  }

  it should "read back a value written" in {
    test(new Memory) { c =>
      c.io.writeAddr.initSource().setSourceClock(c.clock)
      c.io.readAddr.initSource().setSourceClock(c.clock)

      pokeWrite(c, 0, 123)
      c.clock.step()
      pokeRead(c, 0) shouldBe 123
    }
  }

  it should "read back values written to separate addresses" in {
    test(new Memory) { c =>
      c.io.writeAddr.initSource().setSourceClock(c.clock)
      c.io.readAddr.initSource().setSourceClock(c.clock)

      pokeWrite(c, 0, 123)
      c.clock.step()
      pokeWrite(c, 1, 456)
      c.clock.step()
      pokeRead(c, 0) shouldBe 123
      pokeRead(c, 1) shouldBe 456
    }
  }

  it should "read back the previous value in a read/write conflict" in {
    test(new Memory) { c =>
      c.io.writeAddr.initSource().setSourceClock(c.clock)
      c.io.readAddr.initSource().setSourceClock(c.clock)

      pokeWrite(c, 0, 123)
      c.clock.step()

      pokeWrite(c, 0, 456)
      pokeRead(c, 0) shouldBe 123
    }
  }
}
