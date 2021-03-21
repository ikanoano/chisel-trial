package flims

import chisel3._
import chisel3.util._
import scala.collection._

object Config {
  val KeyWidth    = 8
  val ValueWidth  = 4
  val TotalWidth  = KeyWidth + ValueWidth
  val LargestKey  = ((BigInt(1)<<Config.KeyWidth) - 1).U(Config.KeyWidth.W)
}

class KVS extends Bundle {
  val key           = UInt(Config.KeyWidth.W)   // MSB
  val value         = UInt(Config.ValueWidth.W) // LSB
  override def toPrintable: Printable = {
    p"KVS( " +
    p"k=0x${Hexadecimal(key)}, " +
    p"v=0x${Hexadecimal(value)}" +
    p")"
  }
}

// Couple 2 cycles of setI.bits
class RecordCoupler (size: Int) extends Module {
  val io = IO(new Bundle {
    val source    = Vec(size,   DeqIO(new KVS()))
    val coupled   = Vec(size*2, EnqIO(new KVS()))
  })

  val sellsb    = RegInit(true.B)
  // Asssert source's ready only when all coupled.ready is true in order not to allow unaligned enqueue
  val lsbReady  = io.coupled.take(size).map(_.ready)reduce(_&&_)
  val msbReady  = io.coupled.drop(size).map(_.ready)reduce(_&&_)

  // Enqueue into lsb half or into msb half cyclically
  (0 until size).foreach{ i =>
    io.coupled(i     ).bits  := io.source(i).bits
    io.coupled(i+size).bits  := io.source(i).bits

    io.coupled(i     ).valid :=  sellsb && io.source(i).valid
    io.coupled(i+size).valid := ~sellsb && io.source(i).valid

    when (sellsb) {io.source(i).ready := lsbReady}
    .otherwise    {io.source(i).ready := msbReady}
  }
  when(io.source(0).valid && io.source(0).ready) { sellsb := ~sellsb }
}

object RecordCoupler {
  def apply(src: Vec[DecoupledIO[KVS]], size: Int): Vec[DecoupledIO[KVS]] = {
    val rc = Module(new RecordCoupler(size))
    rc.io.source <> src
    rc.io.coupled
  }
}
