package mrv

import org.scalatest._
import org.scalatest.flatspec._
import org.scalatest.matchers.should._

import chisel3._
import chiseltest._

class OneCycleSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "OneCycle"

  it should "test" in {
    test(new OneCycle(Seq())) { c =>
      
    }
  }
}
