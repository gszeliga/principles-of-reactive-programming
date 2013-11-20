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

  def availableRooms(currentRow: Int, currentCol: Int) = {

    def nextCoordinate(current: Int, edge: Int)(reachedTheEdge: Int => Boolean)(next: Int => Int) = {
      if (reachedTheEdge(current)) edge else next(current)
    }

    val aboveRow = nextCoordinate(currentRow, SimConfig.roomRows - 1)(_ == 0) { _ - 1 }
    val belowRow = nextCoordinate(currentRow, 0)(_ == SimConfig.roomRows - 1) { _ + 1 }

    val aboveCol = nextCoordinate(currentCol, SimConfig.roomColumns - 1)(_ == 0) { _ - 1 }
    val belowCol = nextCoordinate(currentCol, 0)(_ == SimConfig.roomColumns - 1) { _ + 1 }

    List((aboveCol, currentRow), (belowCol, currentRow), (currentCol, aboveRow), (currentCol, belowRow))
  }

  trait State {
    def evaluate(currentDay: Int, person: Person)
  }

  object Initial extends State {
    def evaluate(currentDay: Int, person: Person) = {
      val status = if (person.infected) Infected else Healthy
      status.evaluate(currentDay, person)
    }
  }

  object Healthy extends State {
    def evaluate(currentDay: Int, person: Person) = {

      val notVisibleInfectedRooms = availableRooms(person.row, person.col).filter(room => persons.filter(p => p.row == room._2 && p.col == room._1 && (p.sick || p.dead)).isEmpty)

      if (!notVisibleInfectedRooms.isEmpty) {

        val cleanRoom = notVisibleInfectedRooms((notVisibleInfectedRooms.size * random).toInt)

        person.row = cleanRoom._2
        person.col = cleanRoom._1
      }

      val theresInfectedPeople = !persons.filter(p => p.row == person.row && p.col == person.col && p.infected).isEmpty

      if (theresInfectedPeople) {
        person.infected = probabilityOfInfection((100 * random).toInt) == 1

        if (person.infected) {
          person.status = Infected
          println(s"Person ${person.id} got infected")
        } else {
          person.status = Healthy
        }
      } else {
        person.status = Healthy
      }

    }
  }

  object Infected extends State {
    def evaluate(currentDay: Int, person: Person) = {
      person.infectedFor = person.infectedFor + currentDay

      println(s"Person ${person.id} has been infected for ${person.infectedFor}")

      val anyRoom = availableRooms(person.row, person.row)((4 * random).toInt)

      person.row = anyRoom._2
      person.col = anyRoom._1

      person.sick = person.infectedFor >= 6

      if (person.sick) {
        println(s"Person ${person.id} got sick")
        person.status = Sick
      } else if (person.infectedFor >= 16 && person.infectedFor <= 18) {
        println(s"Person ${person.id} is now immune")
        person.immune = true
        person.status = Immune
      } else person.status = Infected
    }
  }

  object Immune extends State {
    def evaluate(currentDay: Int, person: Person) = {

      if (person.infectedFor >= 18) {
        person.immune = false
        person.infected = false
        person.infectedFor = 0
        person.status = Healthy
      }
    }
  }

  object Sick extends State {
    def evaluate(currentDay: Int, person: Person) = {

      person.infectedFor = person.infectedFor + currentDay
      println(s"Person ${person.id} has been sick for ${person.infectedFor}")

      val anyRoom = availableRooms(person.row, person.col)((4 * random).toInt)

      person.row = anyRoom._2
      person.col = anyRoom._1

      person.dead = (person.infectedFor > 14) && (probabilityOfDeath((100 * random).toInt) == 1)

      if (person.dead) {
        println(s"Person ${person.id} died")
        person.status = Dead
      } else person.status = Sick
    }
  }

  object Dead extends State {
    def evaluate(currentDay: Int, person: Person) = {

    }
  }

  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    var infectedFor: Int = 0
    var status: State = Initial

    def nextMove() = ((5 * random).toInt) + 1

    def relocate(days: Int, person: Person): Unit = {

      afterDelay(days)({
        val currentPerson = person
        currentPerson.status.evaluate(days, currentPerson)
        relocate(nextMove, person)
      })
    }

    relocate(nextMove, this)

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
