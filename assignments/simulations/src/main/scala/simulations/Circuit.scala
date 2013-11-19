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

    def getExpectedControlWiresSignals(number: Int, cw: List[Wire]): List[(Int, Int)] = {

      def fillUpWithPendingControlWiresSignals(current: List[Int], pending: Int): List[Int] = {
        if (pending <= 0) current
        else fillUpWithPendingControlWiresSignals(0 :: current, pending - 1)
      }

      def buildControlWiresCombination(number: Int, binary: List[Int]): List[Int] = {
        number match {
          case 0 => fillUpWithPendingControlWiresSignals(0 :: Nil, cw.size - 1)
          case 1 => {
            val tmp = 1 :: binary
            fillUpWithPendingControlWiresSignals(tmp, cw.size - tmp.size)
          }
          case _ => {
            val reminder = if (number % 2 == 0) 0 else 1
            buildControlWiresCombination(number / 2, reminder :: binary)
          }
        }
      }

      if (cw.isEmpty) Nil
      else buildControlWiresCombination(number, Nil).reverse.zipWithIndex
    }

    def joinAllIntermediateWires(availableIntermediateWires: Seq[Wire], exit: Wire, outputIndex: Int) {
      val nil = new Wire
      
      availableIntermediateWires match {

        case Nil => orGate(in, nil, exit) //Since there's no control wires we bridge the signal to the exit
        case output :: Nil => {
          orGate(output, nil, exit)
          probe("Final OR gate on OUT" + outputIndex, output)
          probe("Exit value on OUT" + outputIndex, exit)
        }
        case out1 :: out2 :: tail => {
          val newOut = new Wire
          andGate(out1, out2, newOut)
          probe("Intermediate AND gate", newOut)
          joinAllIntermediateWires(tail :+ newOut, exit, outputIndex)
        }
      }

    }

    out.view.zipWithIndex.map { outputsWithIndex =>
      outputsWithIndex match {
        case (outputWire, outputWireIndex) => joinAllIntermediateWires(getExpectedControlWiresSignals(outputWireIndex, c).map({
          controlWireSignal =>

            val controlWire = c(controlWireSignal._2)
            val intermediateOutputWire = new Wire

            val invertedControlWire = new Wire
            inverter(controlWire, invertedControlWire)

            //If control wire signal must be active
            if (controlWireSignal._1 == 1) {
              andGate(controlWire, in, intermediateOutputWire)
              probe(s"C${controlWireSignal._2} AND with signal", intermediateOutputWire)
            } else {
              andGate(invertedControlWire, in, intermediateOutputWire)
              probe(s"signal and NOT(C${controlWireSignal._2})", intermediateOutputWire)
            }

            intermediateOutputWire

        }), outputWire, outputWireIndex)
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
