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
  * From within sbt use:        testOnly flims.*
  * From a terminal shell use:  sbt 'testOnly flims.*
  */
class SortSpec extends FreeSpec with ChiselScalatestTester {
  val sizepow = 2
  val size    = Seq.fill(sizepow)(2).reduce(_*_) // 2 ** sizepow

  val keyMask   = ((BigInt(1)<<KeyWidth  ) - 1) << ValueWidth
  val valueMask = ((BigInt(1)<<ValueWidth) - 1)
  val totalMask = ((BigInt(1)<<TotalWidth) - 1)
  def tpl2kvs(t:(Int, Int)) = (BigInt(t._1) << ValueWidth) | BigInt(t._2)
  def kvs2tpl(kvs:BigInt)   = ( (kvs>>ValueWidth, kvs & valueMask) )
  def invFlattenKVS(fkvs: BigInt) =
    Seq.tabulate(size)(i => (fkvs >> (TotalWidth*i)) & totalMask).map(kvs2tpl(_))

  def randSeq(s: Int) = Seq.fill(s) (
    // select 1 until max-1
    (Random.nextInt((1 <<   KeyWidth.min(8))-2)+1,
     Random.nextInt((1 << ValueWidth.min(8))-2)+1)
  )

  def backpressure(q: DecoupledIO[KVS], until: () => Boolean, clk: Clock) = {
    // enqueueSeq leads 'Non-enclosed timescopes' if the test ends without enq completed, or leads timeout if the test waits enq
    // xx q.enqueueSeq(Seq.fill(32){ (new KVS).Lit(_.key -> 0.U,  _.value -> 0.U) })

    timescope { // Finishing a test without reverting any poked signal is not allowed: "Mix of ending timescopes and old timescopes"
      q.bits.poke((new KVS).Lit(_.key -> 0.U,  _.value -> 0.U))
      q.valid.poke(true.B)
      while(!until()) clk.step(1)
    }
  }

  class FlimsWrapper (size:Int) extends Module {
    // Wrapping FLiMS which outpus a "Vec of KVS".
    // serialize "Vec of KVS" into UInt to pass it to expect() like verilog does ...
    val io = IO(new Bundle {
      val setI  = Vec(size*2, DeqIO(new KVS()))       // input size-way sequence of sorted KVS
      val setO  = EnqIO(UInt((size*TotalWidth).W))    // output sequence of sorted KVS
      val cycleCounter = Output(UInt(16.W))           // cycle counter
    })
    val inner = Module(new FLiMS(size))
    inner.io.setI       <> io.setI
    io.setO.bits        := inner.io.setO.bits.asUInt  // reinterpret cast
    io.setO.valid       := inner.io.setO.valid
    inner.io.setO.ready := io.setO.ready
    val (counter, wrap)  = Counter(true.B, 65536)
    io.cycleCounter     := counter
  }

  // seqlen = # of KVS per a sequence
  def flimsTest(dut: FlimsWrapper, seqlen: Int, stall: Boolean) = {
      dut.io.setI.foreach{ _.initSource().setSourceClock(dut.clock) }
      dut.io.setO.initSink().setSinkClock(dut.clock)

      val orgSeq      = randSeq(seqlen*size*2)
      val testSeq     = orgSeq.grouped(seqlen).map(_.sorted.reverse).toSeq
      val sortedSeq   = (orgSeq ++ Seq.fill(size)((0,0))).sorted.reverse.toSeq
      var complete    = false

      val startCycle       = dut.io.cycleCounter.peek().litValue()
      val ctx = for (i <- 0 until size*2) yield fork {
        // enqueue kvs
        testSeq(i).map{ case (k,v) => (new KVS).Lit(_.key -> k.U,  _.value -> v.U) }.foreach{ e =>
          if (stall && Random.nextInt(10)==1) dut.clock.step(Random.nextInt(10))
          dut.io.setI(i).enqueue(e)
        }

        // backpressure
        backpressure(dut.io.setI(i), () => complete/*!dut.io.setO.ready.peek().litToBoolean*/, dut.clock)
      }

      fork {
        // expectDequeue* is unavailable to verify a merged sequence since they don't tolerate tie-records
        //dut.io.setO.expectDequeueSeq(sortedSeq.map(_.reverse).map(i => i.map(j => tpl2kvs(j)).reduce((z,n) => (z<<TotalWidth) | n).U))

        timescope {
          dut.io.setO.ready.poke(true.B)  // free run
          // Test peek KVS
          val peekSeq = (for(_ <- 0 until 2*seqlen+1) yield {  // testSeq ++ backpressure expected
            dut.clock.step(1) // mandatory
            if (stall && Random.nextInt(10)==1) dut.clock.step(Random.nextInt(10))
            dut.io.setO.waitForValid()
            invFlattenKVS(dut.io.setO.peek().litValue())
          }).flatMap(i=>i)

          val finishCycle = dut.io.cycleCounter.peek().litValue()
          complete = true
          val elapsed = finishCycle-startCycle
          print(f"Total cycle:   ${elapsed}\n")

          // Check if the output sequence is sorted disregarding value
          assert(peekSeq.map(_._1) == sortedSeq.map(_._1))
          // Check if All KVS exists
          assert(peekSeq.sorted.reverse == sortedSeq)
          // Check if elapsed cycle is reasonabled
          assert(elapsed < (seqlen * 2.3).toInt)
        }

        //print(f"peek:   $peekSeq\n")
        //print(f"sorted: $sortedSeq\n")
      }.join()  // join needed to suppress 'non-enclosed timescope'
      ctx.foreach{_.join()}
  }


  "FLiMS should output a sequence of merged KVS (size KVS/cycle) when it inputs size-way of ones" in {
    test (
      new FlimsWrapper(size)
//    vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  vcd dump enabled
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      flimsTest(dut, 128, false)
    }
  }

  "FLiMS should output a sequence of merged KVS (size KVS/cycle) when it inputs size-way of ones with stall" in {
    test (
      new FlimsWrapper(size)
//    vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv  vcd dump enabled
    ).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      flimsTest(dut, 128, true)
    }
  }

  "ParallelMerger(1) should output a sequence of merged KVS (1 KVS/cycle) e.g. {10 9 8 7 6 5 4 3} when it inputs 2-way of ones (1 KVS/cycle/way) e.g. {9 7 5 3} and {10 8 6 4}" in {
    test (new ParallelMerger(1)) { dut =>
      //init
      dut.io.setI.foreach{ i => i.initSource().setSourceClock(dut.clock) }
      dut.io.setO.foreach{ o => o.initSink().setSinkClock(dut.clock) }
      //dut.io.setO.initSink().setSinkClock(dut.clock)
      var complete = false

      fork {
        // enqueue 9 7 5 3
        for(i <- 0 until 4) dut.io.setI(0).enqueue((new KVS).Lit(_.key -> (9-2*i).U,  _.value -> 9.U))
        backpressure(dut.io.setI(0), () => complete, dut.clock)
      }.fork {
        dut.clock.step(10)
        // enqueue 10 8 6 4
        for(i <- 0 until 4) dut.io.setI(1).enqueue((new KVS).Lit(_.key -> (10-2*i).U, _.value -> 9.U))
        backpressure(dut.io.setI(1), () => complete, dut.clock)
      }.fork {
        // remove head (invalid records)
        dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> LargestKey, _.value -> 0.U))
        dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> LargestKey, _.value -> 0.U))
        // dequeue 10 ... 4
        for(i <- 0 until 8) dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> (10-i).U, _.value -> 9.U))
        // backpressure - all valid KVS are already out
        dut.io.setO(0).expectDequeue((new KVS).Lit(_.key -> 0.U, _.value -> 0.U))

        timescope {
          // Free run otherwise the test for setO(1) timeouts because ParallelMerger will stall due to ~setO(0).ready
          dut.clock.step(1) // required to poke
          dut.io.setO(0).ready.poke(true.B)
          dut.clock.step(5) // required to poke
        }
        complete = true
        dut.clock.step(1)
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
        // chisel doens't seems to support modifying internal values in any chisel instances after they are instantiated
        // expect = 0.U
        //        = Cat(0.U, 1.U)
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



