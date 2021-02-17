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
        io.setO       := inner.io.setO.asUInt             // reinterpret cast
      }
    ) { dut =>
      val keyMask   = ((BigInt(1)<<KeyWidth  ) - 1) << ValueWidth
      val valueMask = ((BigInt(1)<<ValueWidth) - 1)
      val totalMask = ((BigInt(1)<<TotalWidth) - 1)
      def tpl2kvs(t:(Int, Int)) = (BigInt(t._1) << ValueWidth) | BigInt(t._2)
      def kvs2tpl(kvs:BigInt)   = ( (kvs>>ValueWidth, kvs & valueMask) )
      println(f"totalMask = 0x$totalMask%X")

      for(_ <- 1 to 2048) {
        def randHalfSeq() = Seq.fill(size/2)(
          (Random.nextInt(1 << KeyWidth.min(8)  ),
           Random.nextInt(1 << ValueWidth.min(8)))
        )
        //  MSB         LSB
        // {7 5 3 1 2 4 6 8} = {2 4 6 8} ++ {7 5 3 1}
        val dscascSeq = randHalfSeq().sorted.reverse ++ randHalfSeq().sorted
        // {8 7 6 5 4 3 2 1}
        val sortedSeq = dscascSeq.sorted

        // {8 6 4 2 1 3 5 7}
        val pokeInt   = dscascSeq.map(i => tpl2kvs(i)).reduce((z,n) => (z<<TotalWidth) | n)
        // {1 2 3 4 5 6 7 8}
        val exInt     = sortedSeq.map(i => tpl2kvs(i)).reduce((z,n) => (z<<TotalWidth) | n)
        val exIntMaskK= Seq.fill(size)(keyMask)       .reduce((z,n) => (z<<TotalWidth) | n)
        println(f"exIntMaskK = 0x$exIntMaskK%X")

        // None of the following is valid due to 'Error: Not in a UserModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox'
        // exInt  = 0.U
        //        = Cat(0.U, 1.U)
        //        = Cat(r._2.U(ValueWidth.W), r._1.U(KeyWidth.W))
        //        = chiselTypeOf(dut.inner.io.setI(0)).Lit(_.key -> r._1.U, _.value -> r._2.U).asUInt

        dut.io.setI.poke(pokeInt.U) // pokeInt(0) goes to LSB of pokeInt.U
        // dut.clock.step(1) // not required but needed to show printf in dut
        // dut.io.setO.expect(exInt.U)  // expect() can't allow reversed tie-records - records having the same key but different value.

        val peekInt = dut.io.setO.peek().litValue()
        print(f"poke:   0x$pokeInt%X\n")
        print(f"expect: 0x$exInt%X\n")
        print(f"peek:   0x$peekInt%X\n")

        // Check if key is sorted; Ignore value
        assert((peekInt & exIntMaskK) == (exInt & exIntMaskK))

        val peekSeq = Seq.tabulate(size)(i => (peekInt >> (TotalWidth*i)) & totalMask).map(kvs2tpl(_))
        // Check if All KVS exists
        assert(sortedSeq == peekSeq.sorted)



        print(f"\n")
      }
    }
  }
}



