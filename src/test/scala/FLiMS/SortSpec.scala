// See README.md for license details.

package flims

import flims.Config._
import chisel3._
import chisel3.util._
import chisel3.tester._
import org.scalatest.FreeSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random
// for VCD dump
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
//import treadle.{VerboseAnnotation, WriteVcdAnnotation}

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
  val sizepow = 5
  val size    = Seq.fill(sizepow)(2).reduce(_*_) // 2 ** sizepow

  val keyMask   = ((BigInt(1)<<KeyWidth  ) - 1) << ValueWidth
  val valueMask = ((BigInt(1)<<ValueWidth) - 1)
  val totalMask = ((BigInt(1)<<TotalWidth) - 1)
  def tpl2kvs(t:(Int, Int)) = (BigInt(t._1) << ValueWidth) | BigInt(t._2)
  def kvs2tpl(kvs:BigInt)   = ( (kvs>>ValueWidth, kvs & valueMask) )
  def invFlattenKVS(fkvs: BigInt) =
    Seq.tabulate(size)(i => (fkvs >> (TotalWidth*i)) & totalMask).map(kvs2tpl(_))
  def wipeTpl2(t:(Int, Int)) = (t._1, 0)


  def randSeq(s: Int) = Seq.fill(s) (
    // select 1 until max-1
    (Random.nextInt((1 <<   KeyWidth.min(8))-2)+1,
     Random.nextInt((1 << ValueWidth.min(8))-2)+1)
  )
  val largest   = LargestKey

  "FLiMS should output a sequence of merged KVS (size KVS/cycle) when it inputs size-way of ones" in {
    test (
      new Module {
        // Wrapping SortingNetwork which inputs/outpus a "Vec of KVS".
        // deserialize/serialize "Vec of KVS" from/into UInt to pass it to poke()/expect() like verilog does ...
        val io = IO(new Bundle {
          val setI  = Vec(size, DeqIO(new KVS()))         // input size-way sequence of sorted KVS
          val setO  = EnqIO(UInt((size*TotalWidth).W))    // output sequence of sorted KVS
        })
        val inner = Module(new FLiMS(size))
        inner.io.setI       <> io.setI
        io.setO.bits        := inner.io.setO.bits.asUInt  // reinterpret cast
        io.setO.valid       := inner.io.setO.valid
        inner.io.setO.ready := io.setO.ready
      }
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.setI.foreach{ _.initSource().setSourceClock(dut.clock) }
      dut.io.setO.initSink().setSinkClock(dut.clock)

      val seqlen      = 128   // # of KVS per a sequence
      val orgSeq      = randSeq(seqlen*size)
      val testSeq     = orgSeq.grouped(seqlen).map(_.sorted.reverse).toSeq
      val sortedSeq   = (orgSeq ++ Seq.fill(size)((0,0))).sorted.reverse.toSeq

      val ctx = for (i <- 0 until size) yield fork {
        // enqueue kvs
        dut.io.setI(i).enqueueSeq(testSeq(i).map{ case (k,v) => (new KVS).Lit(_.key -> k.U,  _.value -> v.U) })

        // backpressure
        // enqueueSeq leads 'Non-enclosed timescopes' if the test ends without enq, or timeout if the test waits enq
        //dut.io.setI(i).enqueueSeq(Seq.fill(32){ (new KVS).Lit(_.key -> 0.U,  _.value -> 0.U) })
        timescope {
          dut.io.setI(i).bits.poke((new KVS).Lit(_.key -> 0.U,  _.value -> 0.U))
          dut.io.setI(i).valid.poke(true.B)
          dut.clock.step(100)
        }
      }

      fork {
        // !!! Can't use expectDequeue* to verify the merged sequence since they don't tolerate tie-records !!!
        //dut.io.setO.expectDequeueSeq(sortedSeq.map(_.reverse).map(i => i.map(j => tpl2kvs(j)).reduce((z,n) => (z<<TotalWidth) | n).U))

        timescope {
          dut.io.setO.ready.poke(true.B)  // free run
          // Test peek KVS
          val peekSeq = (for(_ <- 0 until seqlen+1) yield {  // testSeq ++ backpressure expected
            dut.clock.step(1)
            dut.io.setO.waitForValid()
            invFlattenKVS(dut.io.setO.peek().litValue())
          }).flatMap(i=>i)

          // Check if key is sorted disregarding value
          assert(peekSeq.map(_._1) == sortedSeq.map(_._1))
          // Check if All KVS exists
          assert(peekSeq.sorted.reverse == sortedSeq)
        }

        //print(f"peek:   $peekSeq\n")
        //print(f"sorted: $sortedSeq\n")
      }.join()  // join needed to suppress 'non-enclosed timescope'
      ctx.foreach{_.join()}

    }
  }

  "ParallelMerger(2) should output a sequence of merged KVS (2 KVS/cycle) e.g. {(10 9) (8 7) (6 5) (4 3)} when it inputs 2-way of ones (1 KVS/cycle/way) e.g. {9 7 5 3} and {10 8 6 4}" in {
    //                          vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  vcd dump enabled
    test (new ParallelMerger(2)) { dut =>
      //init
      dut.io.setI.foreach{ i => i.initSource().setSourceClock(dut.clock) }
      dut.io.setO.foreach{ o => o.initSink().setSinkClock(dut.clock) }
      //dut.io.setO.initSink().setSinkClock(dut.clock)

      fork {
        // enqueue 9 7 5 3
        for(i <- 0 until 4) dut.io.setI(0).enqueue((new KVS).Lit(_.key -> (9-2*i).U,  _.value -> 9.U))
        // backpressure
        for(_ <- 0 until 4) dut.io.setI(0).enqueue((new KVS).Lit(_.key -> 0.U,        _.value -> 9.U))
      }.fork {
        dut.clock.step(10)
        // enqueue 10 8 6 4
        for(i <- 0 until 4) dut.io.setI(1).enqueue((new KVS).Lit(_.key -> (10-2*i).U, _.value -> 9.U))
        // backpressure
        for(_ <- 0 until 4) dut.io.setI(1).enqueue((new KVS).Lit(_.key -> 0.U,        _.value -> 9.U))
      }.fork {
        // remove head (invalid)
        dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> largest, _.value -> 0.U))
        // dequeue 10 8 6 4
        for(i <- 0 until 4) dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> (10-2*i).U, _.value -> 9.U))
        // backpressure - all valid KVS are already out
        dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> 0.U, _.value -> 9.U))

        // Free run otherwise the test for setO(1) timeouts because ParallelMerger will stall due to ~setO(0).ready
        dut.clock.step(1) // required to poke
        dut.io.setO(0).ready.poke(true.B)
        dut.clock.step(1) // required to poke
      }.fork {
        // head
        dut.io.setO(1).expectDequeue((new KVS).Lit(_.key -> largest, _.value -> 0.U))
        // dequeue 9 7 5 3
        for(i <- 0 until 4) dut.io.setO(1).expectDequeue((new KVS).Lit(_.key -> (9-2*i).U, _.value -> 9.U))
        // backpressure
        dut.io.setO(1).expectDequeue((new KVS).Lit(_.key -> 0.U, _.value -> 9.U))

        dut.clock.step(1) // required to poke
        dut.io.setO(1).ready.poke(true.B)
        dut.clock.step(1) // required to poke
      }.join()
    }
  }

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
      for(_ <- 1 to 1024) {
        //  MSB         LSB
        // {7 5 3 1 2 4 6 8} = {2 4 6 8} ++ {7 5 3 1}
        val dscascSeq = randSeq(size/2).sorted.reverse ++ randSeq(size/2).sorted
        // {8 7 6 5 4 3 2 1}
        val sortedSeq = dscascSeq.sorted.reverse
        // {8 6 4 2 1 3 5 7}
        val pokeInt   = dscascSeq.map(i => tpl2kvs(i)).reduce((z,n) => (z<<TotalWidth) | n)

        // None of the following is valid due to 'Error: Not in a UserModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox'
        // exInt  = 0.U
        //        = Cat(0.U, 1.U)
        //        = Cat(r._2.U(ValueWidth.W), r._1.U(KeyWidth.W))
        //        = chiselTypeOf(dut.inner.io.setI(0)).Lit(_.key -> r._1.U, _.value -> r._2.U).asUInt

        dut.io.setI.poke(pokeInt.U) // pokeInt(0) goes to LSB of pokeInt.U
        // dut.clock.step(1) // not required but needed to show printf in dut
        // dut.io.setO.expect(exInt.U)  // expect() can't allow reversed tie-records - records having the same key but different value.

        val peekInt = dut.io.setO.peek().litValue()
        val peekSeq = invFlattenKVS(peekInt)
        //print(f"poke:   $dscascSeq\n")
        //print(f"peek:   $peekSeq\n")
        //print(f"key:    ${peekSeq.map(_._1)}\n")
        //print(f"\n")

        // Check if key is sorted disregarding value
        assert(peekSeq.map(_._1) == sortedSeq.map(_._1))
        // Check if All KVS exists
        assert(peekSeq.sorted.reverse == sortedSeq)

      }
    }
  }
}



