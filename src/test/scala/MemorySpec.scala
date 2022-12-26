package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import chisel3._
import chiseltest._
import chisel3.util._

class MemorySpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory"

  def pokeRead(c: MemoryPort, addr: Int): BigInt = {
    c.readAddr.valid.poke(true.B)
    c.readAddr.bits.poke(addr.U)
    c.readData.peekInt()
  }

  def pokeWrite(c: MemoryPort, addr: Int, value: Int) = {
    c.writeAddr.valid.poke(true.B)
    c.writeAddr.bits.poke(addr.U)
    c.writeData.poke(value.U)
  }

  def waitDone(c: MemorySim) = {
    while (!c.io.loaded.peekBoolean()) {
      c.clock.step(1)
    }
  }

  it should "allow initialisation from a file" in {
    test(new MemorySim(List(123, 456, 0, 0))) { c =>
      c.io.mem.writeAddr.initSource().setSourceClock(c.clock)
      c.io.mem.readAddr.initSource().setSourceClock(c.clock)
      waitDone(c)

      pokeRead(c.io.mem, 0) shouldBe 123
      pokeRead(c.io.mem, 1) shouldBe 456
      pokeRead(c.io.mem, 2) shouldBe 0
      pokeRead(c.io.mem, 3) shouldBe 0
    }
  }

  it should "read back a value written" in {
    test(new MemorySim(List.fill(1)(0))) { c =>
      c.io.mem.writeAddr.initSource().setSourceClock(c.clock)
      c.io.mem.readAddr.initSource().setSourceClock(c.clock)
      waitDone(c)

      pokeWrite(c.io.mem, 0, 123)
      c.clock.step()
      pokeRead(c.io.mem, 0) shouldBe 123
    }
  }

  it should "read back values written to separate addresses" in {
    test(new MemorySim(List.fill(2)(0))) { c =>
      c.io.mem.writeAddr.initSource().setSourceClock(c.clock)
      c.io.mem.readAddr.initSource().setSourceClock(c.clock)
      waitDone(c)

      pokeWrite(c.io.mem, 0, 123)
      c.clock.step()
      pokeWrite(c.io.mem, 1, 456)
      c.clock.step()
      pokeRead(c.io.mem, 0) shouldBe 123
      pokeRead(c.io.mem, 1) shouldBe 456
    }
  }

  it should "read back the previous value in a read/write conflict" in {
    test(new MemorySim(List.fill(1)(0))) { c =>
      c.io.mem.writeAddr.initSource().setSourceClock(c.clock)
      c.io.mem.readAddr.initSource().setSourceClock(c.clock)
      waitDone(c)

      pokeWrite(c.io.mem, 0, 123)
      c.clock.step()

      pokeWrite(c.io.mem, 0, 456)
      pokeRead(c.io.mem, 0) shouldBe 123
    }
  }
}
