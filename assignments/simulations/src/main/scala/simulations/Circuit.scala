package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {

    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal

      afterDelay(OrGateDelay)(output.setSignal(a1Sig || a2Sig))
    }

    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {

    val out1, out2, inverted = new Wire

    inverter(a1, out1)
    inverter(a2, out2)
    andGate(out1, out2, inverted)
    inverter(inverted, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {

    def getExpectedControlWireSignals(source: Int): List[(Int, Int)] = {

      def control(number: Int, binary: List[Int]): List[Int] = {
        if (number == 0) 1 :: Nil
        else if (number == 1) 1 :: binary
        else {
          val reminder = if (number % 2 == 0) 0 else 1
          control(number / 2, reminder :: binary)
        }
      }

      control(source, Nil).reverse.zipWithIndex
    }

    def putAllTogether(outputs: Seq[Wire], exit: Wire) {
      outputs match {

        case Nil =>
        case output :: Nil => {
          orGate(output, exit, exit)
          probe("Final OR gate", output)
          probe("Exit value", exit)
        }
        case out1 :: out2 :: tail => {
          val newOut = new Wire
          andGate(out1, out2, newOut)
          probe("Intermediate AND gate", newOut)
          putAllTogether(tail :+ newOut, exit)
        }
      }

    }

    val inverted = new Wire
    inverter(in, inverted)

    out.view.zipWithIndex.map { ow_with_index =>
      ow_with_index match {
        case (ow, index) => putAllTogether(getExpectedControlWireSignals(index).map({
          cw_with_index =>

            val cw = c(cw_with_index._2)
            val cwout = new Wire

            //If should I consider control signal
            if (cw_with_index._1 == 1) {
              andGate(cw, in, cwout)
              probe(s"C${cw_with_index._2} AND with signal", cwout)
            } else {
              andGate(cw, inverted, cwout)
              probe(s"C${cw_with_index._2} AND with NOT(signal)", cwout)
            }

            cwout

        }), ow)
      }
    }.force

  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
