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

  val persons: List[Person] = createPupulation(300) // to complete: construct list of persons

  def probabilityExpression(probability: Int) = {

    var pending = probability

    (1 to 100) map { i =>
      val flag = (2 * random).toInt

      if (flag == 1 && pending > 0) {
        pending = pending - 1
        flag
      } else 0
    }
  }

  val probabilityOfInfection = probabilityExpression(40)
  val probabilityOfDeath = probabilityExpression(25)

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var infectedAt: Int = 0

    def nextMove() = ((5 * random).toInt) + 1

    def relocate(delay: Int): Unit = {

      afterDelay(delay)({

        val currentPersonsDistribution: List[Person] = persons

        val currentDay = delay
        val currenRow = row
        val currenCol = col

        val isCurrentlyInfected = infected
        val elapsedTimeSinceInfection = if (isCurrentlyInfected) (currentDay - infectedAt) else 0
        val isCurrentlySick = sick
        val isCurrentlyDead = dead
        val isCurrentlyImmune = elapsedTimeSinceInfection >= 16 && elapsedTimeSinceInfection <= 18 && !isCurrentlySick
        val mustBeHealthy = isCurrentlyImmune && elapsedTimeSinceInfection >= 18

        if (mustBeHealthy) {

          infected = false
          sick = false
          immune = false
          infectedAt = 0

          println(s"Person ${id} is health again!")

        } else {

          immune = isCurrentlyImmune

          if (isCurrentlyInfected && !isCurrentlySick && !isCurrentlyImmune) {

            sick = elapsedTimeSinceInfection >= 6

            if (sick) {
              println(s"Person ${id} got sick after ${elapsedTimeSinceInfection} days!!")
            }
          }

          if (isCurrentlySick && !isCurrentlyDead) {

            dead = ((delay - infectedAt) > 14) && (probabilityOfDeath((100 * random).toInt) == 1)

            if (dead) {
              println(s"Person ${id} died after ${elapsedTimeSinceInfection} days")
            }
          }
        }

        if (!dead) {

          def nextCoordinate(current: Int, edge: Int)(reachedTheEdge: Int => Boolean)(next: Int => Int) = {
            if (reachedTheEdge(current)) edge else next(current)
          }

          val aboveRow = nextCoordinate(currenRow, SimConfig.roomRows)(_ == 0) { _ - 1 }
          val belowRow = nextCoordinate(currenRow, 0)(_ == SimConfig.roomRows) { _ + 1 }

          val aboveCol = nextCoordinate(currenCol, SimConfig.roomColumns)(_ == 0) { _ - 1 }
          val belowCol = nextCoordinate(currenCol, 0)(_ == SimConfig.roomColumns) { _ + 1 }

          val availableRooms = List((aboveCol, currenRow), (belowCol, currenRow), (currenCol, aboveRow), (currenCol, belowRow))

          if (!isCurrentlyInfected) {

            val notVisibleInfectedRooms = availableRooms.filter(pos => currentPersonsDistribution.filter(p => p.row == pos._2 && p.col == pos._1 && (p.sick || p.dead) && p.id != id).isEmpty)

            if (!notVisibleInfectedRooms.isEmpty) {

              val cleanRoom = notVisibleInfectedRooms((notVisibleInfectedRooms.size * random).toInt)

              row = cleanRoom._2
              col = cleanRoom._1

              val theresInfectedPeople = !currentPersonsDistribution.filter(p => p.row == row && p.col == col && p.infected && p.id != id).isEmpty

              if (theresInfectedPeople) {
                infected = probabilityOfInfection((100 * random).toInt) == 1

                if (infected) {
                  infectedAt = currentDay
                  println(s"Person ${id} got infected at day ${infectedAt}!!")
                }
              }

              relocate(currentDay + nextMove)
            }
          } else if (isCurrentlyInfected) {
            val anyRoom = availableRooms((availableRooms.size * random).toInt)

            row = anyRoom._2
            col = anyRoom._1

            relocate(currentDay + nextMove)
          }
        }
      })
    }

    relocate(nextMove)

  }

  def createPupulation(size: Int): List[Person] = {

    val luckyOne = (size * random).toInt + 1

    println(s"The initial infected one is '${luckyOne}'")

    def populate(tmp: List[Person], pending: Int): List[Person] =
      {
        if (pending == 0) tmp
        else {
          val p = new Person(pending)
          p.infected = pending == luckyOne
          populate(p :: tmp, pending - 1)
        }
      }

    populate(Nil, size)
  }

}
