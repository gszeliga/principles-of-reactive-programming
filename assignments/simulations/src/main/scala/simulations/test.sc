package simulations

object test {

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
  }                                               //> getControlWireSignals: (source: Int, cws: Int)Seq[(Int, Int)]

  getControlWireSignals(0, 2)                     //> res0: Seq[(Int, Int)] = List((0,0), (0,1))
  getControlWireSignals(1, 2)                     //> res1: Seq[(Int, Int)] = List((1,0), (0,1))
  getControlWireSignals(2, 2)                     //> res2: Seq[(Int, Int)] = List((0,0), (1,1))
  getControlWireSignals(3, 2)                     //> res3: Seq[(Int, Int)] = List((1,0), (1,1))

}