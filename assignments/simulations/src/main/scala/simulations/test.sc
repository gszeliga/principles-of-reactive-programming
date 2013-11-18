package simulations

object test {

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
  }                                               //> getControlWireSignals: (source: Int)Seq[(Int, Int)]

  getControlWireSignals(5)                        //> res0: Seq[(Int, Int)] = List((1,0), (0,1), (1,2))
  getControlWireSignals(9)                        //> res1: Seq[(Int, Int)] = List((1,0), (0,1), (0,2), (1,3))
  getControlWireSignals(10)                       //> res2: Seq[(Int, Int)] = List((0,0), (1,1), (0,2), (1,3))
  getControlWireSignals(0)                        //> res3: Seq[(Int, Int)] = List()
  getControlWireSignals(1)                        //> res4: Seq[(Int, Int)] = List((1,0))
  getControlWireSignals(2)                        //> res5: Seq[(Int, Int)] = List((0,0), (1,1))
  getControlWireSignals(3)                        //> res6: Seq[(Int, Int)] = List((1,0), (1,1))
  getControlWireSignals(15)                       //> res7: Seq[(Int, Int)] = List((1,0), (1,1), (1,2), (1,3))
  getControlWireSignals(11)                       //> res8: Seq[(Int, Int)] = List((1,0), (1,1), (0,2), (1,3))


}