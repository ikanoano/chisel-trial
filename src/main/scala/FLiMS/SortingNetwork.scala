package flims

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import scala.collection._

// Partial bitonic sorter network
//  >>>>>>>  <<<<<<<
// {8 6 4 2  1 3 5 7}
//         v merge
//  <<<<<<<<<<<<<<<
// {1 2 3 4 5 6 7 8}
class SortingNetwork(size: Int) extends Module {
  val io = IO(new Bundle {
    val setI          = Input (Vec(size, new KVS()))
    val setO          = Output(Vec(size, new KVS()))
  })

  assert(size >= 2)

  val cmp   = VecInit.tabulate(size/2)(i => io.setI(i).key>io.setI(i + size/2).key)  // devide records into bigger half and smaller ones
  val big   = VecInit.tabulate(size/2)(i => Mux( cmp(i), io.setI(i), io.setI(i + size/2)))
  val small = VecInit.tabulate(size/2)(i => Mux(!cmp(i), io.setI(i), io.setI(i + size/2)))

  if (size==2) {
    //                 LSB    MSB
    io.setO         := big ++ small
  } else {
    val snSmall = Module(new SortingNetwork(size/2))
    val snBig   = Module(new SortingNetwork(size/2))
    snSmall.io.setI := small
    snBig.io.setI   := big
    //                 LSB(first idx)   MSB(last idx)
    io.setO         := snBig.io.setO ++ snSmall.io.setO
  }

  // override def toPrintable: Printable = {  // Module doesn't extend Printable
  def showState: Printable = {
    p"SortingNetwork($size)\n" +
    p"  setI   : ${io.setI}\n" +
    p"  cmp    : $cmp\n" +
    p"  big    : $big\n" +
    p"  small  : $small\n" +
    p"  setO   : ${io.setO}\n"
  }
  printf(p"${this.showState}")

}
