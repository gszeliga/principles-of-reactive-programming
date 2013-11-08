package com.coursera.porp.w1

object generator_sample {

  	val genInt = new Generator[Int]{
  		val rand = new java.util.Random
  		def generate = rand.nextInt()
  	}                                         //> genInt  : com.coursera.porp.w1.Generator[Int]{val rand: java.util.Random} = 
                                                  //| com.coursera.porp.w1.generator_sample$$anonfun$main$1$$anon$1@32bf7190
  	
  	val genBoolean = new Generator[Boolean]{
  		def generate = genInt.generate > 0
  	}                                         //> genBoolean  : com.coursera.porp.w1.Generator[Boolean] = com.coursera.porp.w1
                                                  //| .generator_sample$$anonfun$main$1$$anon$2@2d342ba4
  	
  	val genPairs = new Generator[(Int, Int)]{
  		def generate = (genInt.generate, genInt.generate)
  	}                                         //> genPairs  : com.coursera.porp.w1.Generator[(Int, Int)] = com.coursera.porp.w
                                                  //| 1.generator_sample$$anonfun$main$1$$anon$3@3da99561
 		 	
  	val booleans = for(x <- genInt) yield x > 0
                                                  //> booleans  : com.coursera.porp.w1.Generator[Boolean] = com.coursera.porp.w1.G
                                                  //| enerator$$anon$1@3f77b3cd
    //expanded to
    genInt map {x => x > 0}                       //> res0: com.coursera.porp.w1.Generator[Boolean] = com.coursera.porp.w1.Generat
                                                  //| or$$anon$1@423e5d1
  	
  	
  	val pairs = genInt flatMap {x => genInt map {y => (x,y)}}
                                                  //> pairs  : com.coursera.porp.w1.Generator[(Int, Int)] = com.coursera.porp.w1.G
                                                  //| enerator$$anon$2@4c5e176f
  	
  	pairs.generate                            //> res1: (Int, Int) = (-642446562,-317093747)
  	pairs.generate                            //> res2: (Int, Int) = (-2108168728,1582033689)
  	
}