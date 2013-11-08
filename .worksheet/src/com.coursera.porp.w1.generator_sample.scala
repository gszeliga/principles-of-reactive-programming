package com.coursera.porp.w1

object generator_sample {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(167); 

  	val genInt = new Generator[Int]{
  		val rand = new java.util.Random
  		def generate = rand.nextInt()
  	};System.out.println("""genInt  : com.coursera.porp.w1.Generator[Int]{val rand: java.util.Random} = """ + $show(genInt ));$skip(92); 
  	
  	val genBoolean = new Generator[Boolean]{
  		def generate = genInt.generate > 0
  	};System.out.println("""genBoolean  : com.coursera.porp.w1.Generator[Boolean] = """ + $show(genBoolean ));$skip(108); 
  	
  	val genPairs = new Generator[(Int, Int)]{
  		def generate = (genInt.generate, genInt.generate)
  	};System.out.println("""genPairs  : com.coursera.porp.w1.Generator[(Int, Int)] = """ + $show(genPairs ));$skip(53); 
 		 	
  	val booleans = for(x <- genInt) yield x > 0;System.out.println("""booleans  : com.coursera.porp.w1.Generator[Boolean] = """ + $show(booleans ));$skip(46); val res$0 = 
    //expanded to
    genInt map {x => x > 0};System.out.println("""res0: com.coursera.porp.w1.Generator[Boolean] = """ + $show(res$0));$skip(69); 
  	
  	
  	val pairs = genInt flatMap {x => genInt map {y => (x,y)}};System.out.println("""pairs  : com.coursera.porp.w1.Generator[(Int, Int)] = """ + $show(pairs ));$skip(22); val res$1 = 
  	
  	pairs.generate;System.out.println("""res1: (Int, Int) = """ + $show(res$1));$skip(18); val res$2 = 
  	pairs.generate;System.out.println("""res2: (Int, Int) = """ + $show(res$2))}
  	
}
