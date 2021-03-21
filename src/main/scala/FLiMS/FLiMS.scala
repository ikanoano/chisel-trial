package flims

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import scala.collection._

/**
  * Merge size-way sequences of input records
  */

class FLiMS (size: Int) extends Module {
  val io = IO(new Bundle {
    val setI  = Vec(size*2, DeqIO(new KVS())) // input size-way sequence of sorted KVS
    val setO  = EnqIO(Vec(size, new KVS()))   // output sequence of sorted KVS
  })

  val pm      = Module(new ParallelMerger(size))

  // input
  pm.io.setI        <> io.setI

  // output: convert Vec(EnqIO(KVS)) into EnqIO(Vec(KVS))
  // TODO: ignore invalid records
  val outputBuffer = pm.io.setO.map(Queue(_, 8))  // -> Vec(deqIO(KVS))
  val isLargestInQueue = outputBuffer(size-1).bits.key === Config.LargestKey

  // All buffer queues takes the common ready signal.
  // outputBuffer(<size/2) must avoid being dequeued until >size/2 ones become valid.
  // If there is a LargestKey, remove it
  outputBuffer.foreach{ _.ready := (io.setO.ready || isLargestInQueue) && outputBuffer(size-1).valid }

  // Valid if outputBuffer is not empty - LargestKey is not counted
  // outputBuffer(*) are all valid if (size-1) is valid
  // LargestKey will be silently dequeued because outputBuffer takes ready==1 even if it emits LargestKey
  io.setO.valid     := outputBuffer(size-1).valid && ~isLargestInQueue
  when(outputBuffer(size-1).valid) { assert(outputBuffer(0).valid===true.B) }  // ensure that

  //io.setO.bits.zip(outputBuffer.map(_.bits)).foreach{ (out, pmout) => out := pmout } // assign bits
  for(i <- 0 until size) {
    io.setO.bits(i) := outputBuffer(i).bits
  }

}

// Decoupled = EnqIO = .ready(in),  .valid(out), .bits(out)
//             DeqIO = .ready(out), .valid(in),  .bits(in)

// Merge sorted sequences at size records per cycle
class ParallelMerger (size: Int) extends Module {
  assert(size.U === (1.U(16.W) << Log2(size.U))) // size must be a power of 2

  val io = IO(new Bundle {
    val setI  = Vec(size*2, DeqIO(new KVS())) // size==2: {{2, 4, 6, 8}, {1, 3, 5, 7}, {2, 3, 4, 5}, {2, 3, 5, 7}}
    val setO  = Vec(size,   EnqIO(new KVS())) // size==2: {1 2, 2 2, 3 3, 3 4, 4 5, 5 5, 6 7, 7 8}
  })

  val setA = Wire(Vec(size, DeqIO(new KVS())))  // input KVS set A
  val setB = Wire(Vec(size, DeqIO(new KVS())))  // input KVS set B

  if (size > 1) {
    val pmA = Module(new ParallelMerger(size/2))
    val pmB = Module(new ParallelMerger(size/2))
    pmA.io.setI   <> io.setI.take(size)
    pmB.io.setI   <> io.setI.drop(size)
    setA          <> RecordCoupler(pmA.io.setO, size/2)
    setB          <> RecordCoupler(pmB.io.setO, size/2)
  } else {
    assert(size == 1)
    setA          <> io.setI.take(1)
    setB          <> io.setI.drop(1)
  }

  // queues storing setA/setB
  val qA  = setA.map(i => Queue(i, 8))  // qA(0) is a DeqIO of a deq
  val qB  = setB.map(i => Queue(i, 8)).reverse
  val qAValid = qA.map(i => i.valid).reduce(_&&_) // all queues in qA are valid
  val qBValid = qB.map(i => i.valid).reduce(_&&_) // TODO checking 0 and size-1 is sufficient

  // head of qA/aB
  val a   = qA.map(i => i.bits)
  val b   = qB.map(i => i.bits)

  // dequeued records to be compared
  // Note that cA and cB are the biggest value right after the reset so that they must be taken before any valid value in qA/qB
  val cA        = RegInit(VecInit(Seq.fill(size)( (new KVS()).Lit(_.key -> Config.LargestKey) )))
  val cB        = RegInit(VecInit(Seq.fill(size)( (new KVS()).Lit(_.key -> Config.LargestKey) )))
  val cmp       = for(i <- 0 until size) yield cA(i).key > cB(i).key

  // stall?
  // For simplicity, all registers will stall if (1) output is not ready or (2) any input queue is not valid
  val isOutReady= io.setO(0).ready  // RecourdCoupler ensures setO(*) is ready if one of them is ready
  val stall     = ~isOutReady || ~qAValid || ~qBValid
  val deqA      = cmp.map( _ && ~stall)
  val deqB      = cmp.map(~_ && ~stall)

  (0 until size).foreach{ i =>
    // dequeue qA/qB into cA/cB
    qA(i).ready := deqA(i);
    qB(i).ready := deqB(i);
    when(deqA(i))  {cA(i) := a(i)}
    when(deqB(i))  {cB(i) := b(i)}
  }

  // sorting network
  val sn        = Module(new SortingNetwork(size))
  // collect half records that are bigger than others
  sn.io.setI   := VecInit.tabulate(size)(i => Mux(cmp(i), cA(i), cB(i)))



  // Output a size-way sequence of sorted KVS.
  // Since SN outputs only size/2-way per cycle while PM outputs size-way,
  // cyclically enqueue them to the LSB size/2 and the MSB size/2.
  (0 until size).foreach{ i =>
    io.setO(i).bits  := sn.io.setO(i)
    io.setO(i).valid := ~stall
  }

    // override def toPrintable: Printable = {  // Module doesn't extend Printable
    def toPrintable: Printable = {  // Module doesn't extend Printable
      val m = //if(stall) {
        p"isOutReady($isOutReady,${io.setO(0).ready},${io.setO(size-1).ready}), " +
        p"qAValid($qAValid)" +
        p"qBValid($qBValid)\n" +
      //} else {
        p"  setI   : ${io.setI}\n" +
        p"  qA($qAValid): $qA\n" +
        p"  qB($qBValid): $qB\n" +
        p"  a      : $a\n" +
        p"  b      : $b\n" +
        p"  cA     : $cA\n" +
        p"  cB     : $cB\n" +
        p"  cmp    : $cmp\n" +
        p"  setO   : ${io.setO}\n"
      //}
      p"ParallelMerger($size)\n" + m
    }
    // printf(p"${toPrintable}")
}

