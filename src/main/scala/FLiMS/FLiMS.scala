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
    val setI  = Vec(size, DeqIO(new KVS()))   // input size-way sequence of sorted KVS
    val setO  = EnqIO(Vec(size, new KVS()))   // output sequence of sorted KVS
  })

  val pm      = Module(new ParallelMerger(size))
  pm.io.setI        := io.setI
  pm.io.setO.foreach{ i/*:EnqIO*/ => i.ready := io.setO.ready }

  io.setO.valid     := pm.io.setO(0).valid;
  io.setO.bits      := pm.io.setO.map{ i/*:EnqIO(KVS)*/ => i.bits }
}

class ParallelMerger (size: Int) extends Module {
  val io = IO(new Bundle {
    val setI  = Vec(size, DeqIO(new KVS()))   // input size-way sequence of sorted KVS set
    val setO  = Vec(size, EnqIO(new KVS()))   // input size-way sequence of sorted KVS set
  })

  val setA = Wire(Vec(size/2, DeqIO(new KVS())))  // input KVS set A
  val setB = Wire(Vec(size/2, DeqIO(new KVS())))  // input KVS set B

  if (size > 2) {
    val pmA = Module(new ParallelMerger(size/2))
    val pmB = Module(new ParallelMerger(size/2))
    pmA.io.setI   := io.setI.take(size/2)
    pmB.io.setI   := io.setI.drop(size/2)
    setA          := pmA.io.setO.map(i => Flipped(i))
    setB          := pmB.io.setO.map(i => Flipped(i))
  } else {
    assert(size == 2)
    setA          := io.setI.take(1)
    setB          := io.setI.drop(1)
  }

  // queues storing setA/setB
  val qA  = setA.map(i => Queue(i, 8))  // qA(0) is a DeqIO of a deq
  val qB  = setB.map(i => Queue(i, 8)).reverse
  val qAValid = qA.map(i => i.valid).reduce(_&&_) // all queues in qA are valid
  val qBValid = qB.map(i => i.valid).reduce(_&&_)

  // head of qA/aB
  val a   = qA.map(i => i.bits)
  val b   = qB.map(i => i.bits)

  // dequeued records to be compared
  // Note that cA and cB are the biggest value right after the reset so that they must be taken before any valid value in qA/qB
  val cA        = Vec(size, RegInit((new KVS()).Lit(_.key -> ~(0.U(Config.KeyWidth.W)))))
  val cB        = Vec(size, RegInit((new KVS()).Lit(_.key -> ~(0.U(Config.KeyWidth.W)))))
  val cmp       = for(i <- 0 until size) yield cA(i).key > cB(i).key

  // stall?
  // For simplicity, all registers will stall if (1) output is not ready or (2) any input queue is not valid
  val isOutReady= io.setO(0).ready && io.setO(size-1).ready // either FIFO is dequeued first
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

  // output a size-way KVS set
  (0 until size).foreach{ i =>
    io.setO(i).bits  := sn.io.setO(i)
    io.setO(i).valid := ~stall
  }
}

