
package gcd

import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

object Elaborate {
  def main(args: Array[String]): Unit = {
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
          Seq(ChiselGeneratorAnnotation(() => new GCD())))
    //chisel3.Driver.execute(args, () => new GCD())
  }
}
