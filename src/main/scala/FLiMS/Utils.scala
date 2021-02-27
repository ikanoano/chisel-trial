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

