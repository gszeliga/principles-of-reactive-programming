package com.coursera.porp.w1

object tree_generator {

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }                                               //> integers  : com.coursera.porp.w1.Generator[Int]{val rand: java.util.Random} 
                                                  //| = com.coursera.porp.w1.tree_generator$$anonfun$main$1$$anon$1@509df6f1

  val booleans = integers map (_ > 0)             //> booleans  : com.coursera.porp.w1.Generator[Boolean] = com.coursera.porp.w1.G
                                                  //| enerator$$anon$1@32ef2c60

  def leafs = for (x <- integers) yield new Leaf(x)
                                                  //> leafs: => com.coursera.porp.w1.Generator[com.coursera.porp.w1.Leaf]

  def inners: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right)                      //> inners: => com.coursera.porp.w1.Generator[com.coursera.porp.w1.Inner]

  def trees = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree                                    //> trees: => com.coursera.porp.w1.Generator[Product with Serializable with com.
                                                  //| coursera.porp.w1.Tree]

  
	trees.generate                            //> res0: Product with Serializable with com.coursera.porp.w1.Tree = Inner(Inner
                                                  //| (Inner(Inner(Inner(Leaf(-237468371),Inner(Leaf(-1685306282),Leaf(1385310464)
                                                  //| )),Inner(Leaf(-1112344220),Inner(Inner(Leaf(-2137614819),Inner(Leaf(-2075562
                                                  //| 790),Inner(Leaf(-316620173),Inner(Leaf(-218660394),Leaf(1360517980))))),Leaf
                                                  //| (-317161981)))),Inner(Leaf(-165021626),Leaf(385558006))),Inner(Leaf(17938419
                                                  //| 80),Leaf(-1553593484))),Leaf(-508507328))
}