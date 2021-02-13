// See README.md for license details.

package flims

import flims.Config._
import chisel3._
import chisel3.util._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

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
  val size = 4

  "SortingNetwork should output a soted seqeunce of records which SN takes at the same cycle" in {
    test (
      new Module {
        // Wrapping SortingNetwork which inputs/outpus a "Vec of KVS".
        // deserialize/serialize "Vec of KVS" from/into UInt to pass it to poke()/expect() like verilog does ...
        val io = IO(new Bundle {
          val setI = Input (UInt((size*TotalWidth).W))
          val setO = Output(UInt((size*TotalWidth).W))
        })
        val inner = Module(new SortingNetwork(size))
        inner.io.setI := io.setI.asTypeOf(inner.io.setI)  // reinterpret cast
        io.setO := inner.io.setO.asUInt                   // reinterpret cast
      }
    ) { dut =>
      for(_ <- 1 to 100) {
        def randHalfSeq() = Seq.fill(size/2)(
          (Random.nextInt(1 << KeyWidth.min(8)  ),
           Random.nextInt(1 << ValueWidth.min(8)))
        )
        //  MSB         LSB
        // {7 5 3 1 2 4 6 8} = {2 4 6 8} ++ {7 5 3 1}
        val dscascSeq = randHalfSeq().sorted.reverse ++ randHalfSeq().sorted
        // {8 7 6 5 4 3 2 1}
        val sortedSeq = dscascSeq.sorted

        def tpl2kvs(t:(Int, Int)) = BigInt(t._1 << ValueWidth) | BigInt(t._2)
        // {8 6 4 2 1 3 5 7}
        val pokeInt   = dscascSeq.map(i => tpl2kvs(i)).reduce((z,n) => (z<<TotalWidth) | n)
        // {1 2 3 4 5 6 7 8}
        val exInt     = sortedSeq.map(i => tpl2kvs(i)).reduce((z,n) => (z<<TotalWidth) | n)

        // None of the following is valid due to 'Error: Not in a UserModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox'
        // exInt  = 0.U
        //        = Cat(0.U, 1.U)
        //        = Cat(r._2.U(ValueWidth.W), r._1.U(KeyWidth.W))
        //        = chiselTypeOf(dut.inner.io.setI(0)).Lit(_.key -> r._1.U, _.value -> r._2.U).asUInt

        dut.io.setI.poke(pokeInt.U) // pokeInt(0) goes to LSB of pokeInt.U
        print(f"poke:   0x$pokeInt%X\n")
        dut.clock.step(1)
        print(f"expect: 0x$exInt%X\n")
        dut.io.setO.expect(exInt.U)
        print(f"\n")
      }
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



