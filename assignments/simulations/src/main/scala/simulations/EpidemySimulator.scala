package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = createPupulation(SimConfig.population) // to complete: construct list of persons

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def changePosition(delay: Int): Unit = {

      afterDelay(delay)({

        val currenRow = row
        val currenCol = col

        val aboveRow = if ((currenRow - 1) <= 0) SimConfig.roomRows else currenRow - 1
        val belowRow = if ((currenRow + 1) >= SimConfig.roomRows) 1 else currenRow + 1

        val aboveCol = if ((currenCol - 1) <= 0) SimConfig.roomColumns else currenCol - 1
        val belowCol = if ((currenCol + 1) >= SimConfig.roomColumns) 1 else currenCol + 1

        row = if (Random.nextInt >= 0) aboveRow else belowRow
        col = if (Random.nextInt >= 0) aboveCol else belowCol

        changePosition(delay + (5 * random).toInt)
        
      })
    }
    
    changePosition((5 * random).toInt)
    //
    // to complete with simulation logic
    //
  }

  def createPupulation(size: Int): List[Person] = {

    def populate(tmp: List[Person], pending: Int): List[Person] =
      {
        if (pending == 0) tmp
        else populate(new Person(pending) :: tmp, pending - 1)
      }

    populate(Nil, size)
  }

}
