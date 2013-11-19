package simulations

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(702); 

  def getControlWireSignals(source: Int, cws: Int) = {

    def completeWiresSignals(current: List[Int], pending: Int): List[Int] = {
      if (pending <= 0) current
      else completeWiresSignals(0 :: current, pending - 1)
    }

    def control(current: Int, binary: List[Int]): List[Int] = {
      if (current == 0) completeWiresSignals(0 :: Nil, cws - 1)
      else if (current == 1) {
        val tmp = 1 :: binary
        completeWiresSignals(tmp, cws - tmp.size)
      } else {
        val reminder = if (current % 2 == 0) 0 else 1
        control(current / 2, reminder :: binary)
      }
    }
		
    control(source, Nil).reverse.view.zipWithIndex.force
  };System.out.println("""getControlWireSignals: (source: Int, cws: Int)Seq[(Int, Int)]""");$skip(31); val res$0 = 

  getControlWireSignals(0, 2);System.out.println("""res0: Seq[(Int, Int)] = """ + $show(res$0));$skip(30); val res$1 = 
  getControlWireSignals(1, 2);System.out.println("""res1: Seq[(Int, Int)] = """ + $show(res$1));$skip(30); val res$2 = 
  getControlWireSignals(2, 2);System.out.println("""res2: Seq[(Int, Int)] = """ + $show(res$2));$skip(30); val res$3 = 
  getControlWireSignals(3, 2);System.out.println("""res3: Seq[(Int, Int)] = """ + $show(res$3))}

}
