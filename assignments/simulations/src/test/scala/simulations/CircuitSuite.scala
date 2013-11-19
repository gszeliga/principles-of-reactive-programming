package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {

    val in1, in2, out = new Wire

    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)

    run

    assert(out.getSignal == false, "initial value is wrong")

    in1.setSignal(true)

    run

    assert(out.getSignal == true, "in1 switched to true")

    in1.setSignal(false)
    in2.setSignal(true)

    run

    assert(out.getSignal == true, "in2 switched to true")

    in1.setSignal(true)
    in2.setSignal(true)

    run

    assert(out.getSignal == true, "both switched to true")
  }

  test("orGate2 example") {

    val in1, in2, out = new Wire

    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)

    run

    assert(out.getSignal == false, "initial value is wrong")

    in1.setSignal(true)

    run

    assert(out.getSignal == true, "in1 switched to true")

    in1.setSignal(false)
    in2.setSignal(true)

    run

    assert(out.getSignal == true, "in2 switched to true")

    in1.setSignal(true)
    in2.setSignal(true)

    run

    assert(out.getSignal == true, "both switched to true")
  }

  test("demux C0-OUT1 example") {

    val s, out = new Wire

    demux(s, Nil, List(out))

    s.setSignal(false)

    run

    assert(out.getSignal == false, "when input is off")

    s.setSignal(true)
    run

    assert(out.getSignal == true, "when input is on")

  }

  test("demux C1-OUT1 example") {

    val s, c0, out1, out2 = new Wire

    demux(s, List(c0), List(out1, out2))

    s.setSignal(true)

    run

    assert(out1.getSignal == true, "when input is on and C0 is off on output 1")
    assert(out2.getSignal == false, "when input is on and C0 is off on output 2")

    s.setSignal(true)
    c0.setSignal(true)
    run

    assert(out1.getSignal == false, "when input is on and C0 is on on output 1")
    assert(out2.getSignal == true, "when input is on and C0 is on on output 2")


  }
  
  test("demux C2-OUT4 example") {

    val s, c0,c1, out0, out1, out2, out3 = new Wire

    demux(s, List(c0, c1), List(out0, out1, out2,out3))

    s.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    
    run

    assert(out0.getSignal == true, "when input is on and C0-C1 off on output 1")
    assert(out1.getSignal == false, "when input is on and C0-C1 off on output 2")
    assert(out2.getSignal == false, "when input is on and C0-C1 off on output 3")
    assert(out3.getSignal == false, "when input is on and C0-C1 off on output 4")
    
    c0.setSignal(true)
    c1.setSignal(false)    
    run
    
    assert(out0.getSignal == false, "when input is on C0(on) and C1 (off) on output 1")
    assert(out1.getSignal == true, "when input is on C0(on) and C1 (off) on output 2")
    assert(out2.getSignal == false, "when input is C0(on) and C1 (off) on output 3")
    assert(out3.getSignal == false, "when input is C0(on) and C1 (off) on output 4")    
    
    c0.setSignal(false)
    c1.setSignal(true)
    run
    
    assert(out0.getSignal == false, "when input is on C0(off) and C1 (on) on output 1")
    assert(out1.getSignal == false, "when input is on C0(off) and C1 (on) on output 2")
    assert(out2.getSignal == true, "when input is C0(off) and C1 (on) on output 3")
    assert(out3.getSignal == false, "when input is C0(off) and C1 (on) on output 4")       
    
    
    c0.setSignal(true)
    c1.setSignal(true)
    run
    
    assert(out0.getSignal == false, "when input is on C0(on) and C1 (on) on output 1")
    assert(out1.getSignal == false, "when input is on C0(on) and C1 (on) on output 2")
    assert(out2.getSignal == false, "when input is C0(on) and C1 (on) on output 3")
    assert(out3.getSignal == true, "when input is C0(on) and C1 (on) on output 4")    
  }  

}
