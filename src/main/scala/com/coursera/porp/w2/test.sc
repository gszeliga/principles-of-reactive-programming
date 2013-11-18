package com.coursera.porp.w2

import week2.Simulation
import week2.Circuits
import week2.Parameters

object test {
  
  object sim extends Circuits with Parameters
  
  import sim._
  
  val in1, in2, sum, carry = new Wire             //> in1  : com.coursera.porp.w2.test.sim.Wire = week2.Gates$Wire@32bf7190
                                                  //| in2  : com.coursera.porp.w2.test.sim.Wire = week2.Gates$Wire@1cb8deef
                                                  //| sum  : com.coursera.porp.w2.test.sim.Wire = week2.Gates$Wire@2d342ba4
                                                  //| carry  : com.coursera.porp.w2.test.sim.Wire = week2.Gates$Wire@3c1d332b
                                                  
  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)                               //> sum 0 new-value = false
  probe("carry", carry)                           //> carry 0 new-value = false
  
  in1 setSignal true
  run()                                           //> *** simulation started, time = 0 ***
                                                  //| sum 5 new-value = true
                                                  //| sum 10 new-value = false
                                                  //| sum 10 new-value = true
  in2 setSignal true
  run()                                           //> *** simulation started, time = 10 ***
                                                  //| carry 13 new-value = true
                                                  //| sum 18 new-value = false
  in1 setSignal false
  run()                                           //> *** simulation started, time = 18 ***
                                                  //| carry 21 new-value = false
                                                  //| sum 26 new-value = true
 
 
}