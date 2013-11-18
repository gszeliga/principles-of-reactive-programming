package com.coursera.porp.w2

import week2.Simulation
import week2.Circuits
import week2.Parameters

object test {
  
  object sim extends Circuits with Parameters
  
  import sim._;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(222); 
  
  val in1, in2, sum, carry = new Wire;System.out.println("""in1  : com.coursera.porp.w2.test.sim.Wire = """ + $show(in1 ));System.out.println("""in2  : com.coursera.porp.w2.test.sim.Wire = """ + $show(in2 ));System.out.println("""sum  : com.coursera.porp.w2.test.sim.Wire = """ + $show(sum ));System.out.println("""carry  : com.coursera.porp.w2.test.sim.Wire = """ + $show(carry ));$skip(85); 
                                                  
  halfAdder(in1, in2, sum, carry);$skip(20); 
  probe("sum", sum);$skip(24); 
  probe("carry", carry);$skip(24); 
  
  in1 setSignal true;$skip(8); 
  run();$skip(21); 
  in2 setSignal true;$skip(8); 
  run();$skip(22); 
  in1 setSignal false;$skip(8); 
  run()}
 
 
}
