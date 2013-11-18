package simulations

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(396); 

  def getControlWireSignals(source: Int) = {

    def control(index: Int, binary: List[Int]): List[Int] = {
      if (index == 0) Nil
      else if (index == 1) 1 :: binary
      else {
        val reminder = if (index % 2 == 0) 0 else 1
        control(index / 2, reminder :: binary)
      }
    }

    control(source, Nil).reverse.view.zipWithIndex.force
  };System.out.println("""getControlWireSignals: (source: Int)Seq[(Int, Int)]""");$skip(28); val res$0 = 

  getControlWireSignals(5);System.out.println("""res0: Seq[(Int, Int)] = """ + $show(res$0));$skip(27); val res$1 = 
  getControlWireSignals(9);System.out.println("""res1: Seq[(Int, Int)] = """ + $show(res$1));$skip(28); val res$2 = 
  getControlWireSignals(10);System.out.println("""res2: Seq[(Int, Int)] = """ + $show(res$2));$skip(27); val res$3 = 
  getControlWireSignals(0);System.out.println("""res3: Seq[(Int, Int)] = """ + $show(res$3));$skip(27); val res$4 = 
  getControlWireSignals(1);System.out.println("""res4: Seq[(Int, Int)] = """ + $show(res$4));$skip(27); val res$5 = 
  getControlWireSignals(2);System.out.println("""res5: Seq[(Int, Int)] = """ + $show(res$5));$skip(27); val res$6 = 
  getControlWireSignals(3);System.out.println("""res6: Seq[(Int, Int)] = """ + $show(res$6));$skip(28); val res$7 = 
  getControlWireSignals(15);System.out.println("""res7: Seq[(Int, Int)] = """ + $show(res$7));$skip(28); val res$8 = 
  getControlWireSignals(11);System.out.println("""res8: Seq[(Int, Int)] = """ + $show(res$8))}


}
