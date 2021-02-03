// See README.md for license details.

package flims

import flims.Config._
import chisel3._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly FLiMS.FLiMSTester
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly FLiMS.FLiMSTester
  * }}}
  */
class SortSpec extends FreeSpec with ChiselScalatestTester {
  val size = 8

  "SortingNetwork should output a soted seqeunce of records which SN takes at the same cycle" in {
    test (
      new Module {
        // This is a Module wrapping SortingNetwork which requires a Vec of KVS while it's not a type poke()/expect() accepts.
        val io = IO(new Bundle {
          val setI = Input (UInt((size*TotalWidth).W))
          val setO = Output(UInt((size*TotalWidth).W))
        })
        val inner = Module(new SortingNetwork(size))
        inner.io.setI := io.setI.asTypeOf(inner.io.setI)  // reinterpret cast
        io.setO := inner.io.setO.asUInt                   // reinterpret cast
      }
    ) { dut =>
      val testVec = (VecInit.tabulate(size)(i => chiselTypeOf(dut.inner.io.setI(0)).Lit(_.key -> (size - 1 - i).U, _.value -> 0.U))).asUInt

      dut.io.setI.poke(testVec)
      dut.io.setO.expect(testVec)

    }
  }

  //"SortingNetwork should output a soted seqeunce of records which SN takes at the same cycle" in {
  //  test(new DecoupledGcd(16)) { dut =>
  //    dut.input.initSource()
  //    dut.input.setSourceClock(dut.clock)
  //    dut.output.initSink()
  //    dut.output.setSinkClock(dut.clock)

  //    val testValues = for { x <- 0 to 10; y <- 0 to 10} yield (x, y)
  //    val inputSeq = testValues.map { case (x, y) => (new GcdInputBundle(16)).Lit(_.value1 -> x.U, _.value2 -> y.U) }
  //    val resultSeq = testValues.map { case (x, y) =>
  //      (new GcdOutputBundle(16)).Lit(_.value1 -> x.U, _.value2 -> y.U, _.gcd -> BigInt(x).gcd(BigInt(y)).U)
  //    }

  //    fork {
  //      // push inputs into the calculator, stall for 11 cycles one third of the way
  //      val (seq1, seq2) = inputSeq.splitAt(resultSeq.length / 3)
  //      dut.input.enqueueSeq(seq1)
  //      dut.clock.step(11)
  //      dut.input.enqueueSeq(seq2)
  //    }.fork {
  //      // retrieve computations from the calculator, stall for 10 cycles one half of the way
  //      val (seq1, seq2) = resultSeq.splitAt(resultSeq.length / 2)
  //      dut.output.expectDequeueSeq(seq1)
  //      dut.clock.step(10)
  //      dut.output.expectDequeueSeq(seq2)
  //    }.join()

  //  }
  //}
}



