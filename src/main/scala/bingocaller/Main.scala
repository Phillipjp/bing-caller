package bingocaller

import bingocaller.Caller.getCallOuts

object Main {

  def main(args: Array[String]): Unit = {
    val callOuts = getCallOuts("call-outs.txt")
    val voice = "Daniel"
    Caller.play((1 to 90), callOuts, voice)
  }
}
