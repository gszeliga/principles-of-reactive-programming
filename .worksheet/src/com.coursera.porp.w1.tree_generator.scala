package com.coursera.porp.w1

object tree_generator {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(166); 

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  };System.out.println("""integers  : com.coursera.porp.w1.Generator[Int]{val rand: java.util.Random} = """ + $show(integers ));$skip(39); 

  val booleans = integers map (_ > 0);System.out.println("""booleans  : com.coursera.porp.w1.Generator[Boolean] = """ + $show(booleans ));$skip(53); 

  def leafs = for (x <- integers) yield new Leaf(x);System.out.println("""leafs: => com.coursera.porp.w1.Generator[com.coursera.porp.w1.Leaf]""");$skip(106); 

  def inners: Generator[Inner] = for {
    left <- trees
    right <- trees
  } yield Inner(left, right);System.out.println("""inners: => com.coursera.porp.w1.Generator[com.coursera.porp.w1.Inner]""");$skip(101); 

  def trees = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree;System.out.println("""trees: => com.coursera.porp.w1.Generator[Product with Serializable with com.coursera.porp.w1.Tree]""");$skip(20); val res$0 = 

  
	trees.generate;System.out.println("""res0: Product with Serializable with com.coursera.porp.w1.Tree = """ + $show(res$0))}
}
