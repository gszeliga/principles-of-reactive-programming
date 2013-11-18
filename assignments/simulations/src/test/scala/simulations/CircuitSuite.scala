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
    
    val s, c0, out = new Wire
    
    demux(s, List(c0), List(out))
    
    s.setSignal(false)
    c0.setSignal(false)
    
    run
    
    assert(out.getSignal == false, "when input is off")
    
    s.setSignal(true)
    c0.setSignal(true)
    run
    
    assert(out.getSignal == true, "when input is on")
    
    s.setSignal(true)
    c0.setSignal(false)
    run
    
    assert(out.getSignal == false, "when input is on but control is off")        
    
    s.setSignal(false)
    c0.setSignal(true)
    run
    
    assert(out.getSignal == false, "when input is off but control is on")    
    
  }  
  
  //
  // to complete with tests for orGate, demux, ...
  //

}
