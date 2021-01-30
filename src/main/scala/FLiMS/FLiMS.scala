// See README.md for license details.

package flims

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import scala.collection._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */

object Config {
  val KeyWidth = 32
  val ValueWidth = 32
}
class KVS extends Bundle {
  val key           = UInt(Config.KeyWidth.W)
  val value         = UInt(Config.ValueWidth.W)
}


class FLiMS (size: Int) extends Module {
  val io = IO(new Bundle {
    val setI  = Vec(size, Flipped(Decoupled(new KVS()))) // input KVS set A
    val setO  = Decoupled(Vec(size, new KVS()))          // output KVS set
  })

  val pm      = Module(new ParallelMerger(size))

  pm.io.setI        := io.setI
  pm.io.setO.foreach{ i => i.ready := io.setO.ready }
  io.setO.valid     := pm.io.setO(0).valid;
  io.setO.bits      := pm.io.setO.map{ i => i.bits }
}

class ParallelMerger (size: Int) extends Module {
  val io = IO(new Bundle {
    val setI  = Vec(size, Flipped(Decoupled(new KVS()))) // input KVS set A
    val setO  = Vec(size,         Decoupled(new KVS()))  // output KVS set
  })

  val setA = Wire(Vec(size/2, Flipped(Decoupled(new KVS()))))  // input KVS set A
  val setB = Wire(Vec(size/2, Flipped(Decoupled(new KVS()))))  // input KVS set B

  if (size > 2) {
    val pmA = Module(new ParallelMerger(size/2))
    val pmB = Module(new ParallelMerger(size/2))
    pmA.io.setI  := io.setI.take(size/2)
    pmB.io.setI  := io.setI.drop(size/2)
    setA                        := pmA.io.setO.map(i => Flipped(i))
    setB                        := pmB.io.setO.map(i => Flipped(i))
  } else {
    assert(size == 2)
    setA      := io.setI.take(1)
    setB      := io.setI.drop(1)
  }

  // queues storing setA/setB
  val qA  = setA.map(i => Queue(i, 8))  // qA(0) is a DecoupledIO
  val qB  = setB.map(i => Queue(i, 8))

  // head of qA/aB
  val a   = qA.map(i => i.bits)
  val b   = qB.map(i => i.bits)
  val cmp = for(i <- 0 until size) yield a(i).key > b.reverse(i).key

  // dequeue qA/qB into cA/cB
  //qA.ready :=  cmp(size-1)
  //qB.ready := !cmp(0)
  (0 until size).foreach{ i =>
    qA(i).ready :=  cmp(i)
    qB(i).ready := !cmp(i)
  }

  // dequeued records to be compared
  val cA    = Vec(size, RegInit((new KVS()).Lit(_.key -> ~(0.U(Config.KeyWidth.W)))))
  val cB    = Vec(size, RegInit((new KVS()).Lit(_.key -> ~(0.U(Config.KeyWidth.W)))))
  //val cA    = Reg(Vec(size, new KVS()))
  //val cB    = Reg(Vec(size, new KVS()))
  // update if cA(i) or cB(i) has taken
  (0 until size).foreach{ i =>
    when(cmp(i))  {cA(i) := a(i)}
    .otherwise    {cB(i) := b.reverse(i)}
  }

  // sorting network
  val sn        = Module(new SortingNetwork(size))
  // collect half records that are bigger than others
  sn.io.setI   := VecInit.tabulate(size)(i => Mux(cmp(i), cA(i), cB(i)))

  // output a KVS bundle if setO for all records is ready
  val isOutReady  = io.setO.map(i => i.ready).reduce((z,n) => z & n)
  (0 until size).foreach{ i =>
    io.setO(i).bits := sn.io.setO(i)
    io.setO(i).valid:= isOutReady
  }
}


class SortingNetwork(size: Int) extends Module {
  val io = IO(new Bundle {
    val setI          = Input(Vec(size, new KVS())) // input KVS set
    val setO          = Output(Vec(size, new KVS())) // output KVS set
  })

  if (size==1) {
    io.setO := io.setI
  } else {
    val cmp   = VecInit.tabulate(size/2)(i => io.setI(i).key>io.setI(i + size/2).key)  // devide records into bigger half and smaller ones
    val big   = VecInit.tabulate(size/2)(i => Mux( cmp(i), io.setI(i), io.setI(size/2)))
    val small = VecInit.tabulate(size/2)(i => Mux(!cmp(i), io.setI(i), io.setI(size/2)))

    val snSmall = Module(new SortingNetwork(size/2))
    val snBig   = Module(new SortingNetwork(size/2))
    snSmall.io.setI := big
    snBig.io.setI   := small

    io.setO := snSmall.io.setO ++ snBig.io.setO
  }
}
