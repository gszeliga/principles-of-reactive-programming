package simulations

import math.random
import scala.util.Random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
  }

  import SimConfig._

  val persons: List[Person] = createPupulation(300) // to complete: construct list of persons

  for (p <- persons) p.start

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

    def evolve(currentDay: Int, person: Person) = {
      move(person)
      val newState = evaluate(currentDay, person)
      newState.apply(person)
      person.status = newState
    }

    protected def apply(person: Person)
    protected def move(person: Person)
    protected def evaluate(currentDay: Int, person: Person): State
  }

  object Healthy extends State {
    def evaluate(currentDay: Int, person: Person) = {

      val theresInfectedPeople = !persons.filter(p => p.row == person.row && p.col == person.col && p.infected).isEmpty

      if (theresInfectedPeople && ((randomBelow(100) + 1) <= 40)) {
        Infected
      } else this

    }

    def move(person: Person) = {
      val notVisibleInfectedRooms = availableRooms(person.row, person.col).filter(room => persons.filter(p => p.row == room._2 && p.col == room._1 && (p.sick || p.dead)).isEmpty)

      if (!notVisibleInfectedRooms.isEmpty) {

        val cleanRoom = notVisibleInfectedRooms((notVisibleInfectedRooms.size * random).toInt)

        person.row = cleanRoom._2
        person.col = cleanRoom._1
      }
    }

    def apply(person: Person) {
      person.infectedFor = 0
      person.dead = false
      person.infected = false
      person.immune = false
      person.sick = false
    }

  }

  object Infected extends State {

    def evaluate(currentDay: Int, person: Person) = {

      person.infectedFor = person.infectedFor + currentDay

      if (person.infectedFor > 16 && person.infectedFor <= 18) {
        Immune
      } else if (person.infectedFor > 6) {
        Sick
      } else if ((person.infectedFor > 14) && ((randomBelow(100) + 1) <= 25)) {
        Dead
      } else this

    }

    def move(person: Person) = {
      val anyRoom = availableRooms(person.row, person.row)((4 * random).toInt)

      person.row = anyRoom._2
      person.col = anyRoom._1
    }

    def apply(person: Person) = {
      person.dead = false
      person.infected = true
      person.immune = false
      person.sick = false
    }
  }

  object Sick extends State {

    def evaluate(currentDay: Int, person: Person) = {

      person.infectedFor = person.infectedFor + currentDay

      if (person.infectedFor > 16 && person.infectedFor <= 18) {
        Immune
      } else if ((person.infectedFor > 14) && ((randomBelow(100) + 1) <= 25)) {
        Dead
      } else this

    }

    def move(person: Person) = {
      val anyRoom = availableRooms(person.row, person.row)((4 * random).toInt)

      person.row = anyRoom._2
      person.col = anyRoom._1
    }

    def apply(person: Person) = {
      person.dead = false
      person.infected = true
      person.immune = false
      person.sick = true
    }
  }

  object Immune extends State {
    def evaluate(currentDay: Int, person: Person) = {

      person.infectedFor = person.infectedFor + currentDay

      if (person.infectedFor > 18) {
        Healthy
      } else this
    }

    def apply(person: Person) {
      person.dead = false
      person.infected = true
      person.immune = true
      person.sick = false
    }

    def move(person: Person) = {
      val anyRoom = availableRooms(person.row, person.row)((4 * random).toInt)

      person.row = anyRoom._2
      person.col = anyRoom._1
    }

  }

  object Dead extends State {
    def evaluate(currentDay: Int, person: Person) = {
      this
    }

    def apply(person: Person) {
      person.dead = true
      person.infected = false
      person.immune = false
      person.sick = false
    }

    def move(person: Person) = {}

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
    var status: State = Healthy

    private def nextCheck() = randomBelow(5) + 1

    private def waitUntil(days: Int): Unit = {

      afterDelay(days)({
        status.evolve(days, this)
        waitUntil(nextCheck)
      })
    }

    def start = { waitUntil(0) }

  }

  def createPupulation(size: Int): List[Person] = {

    def populate(tmp: List[Person], pending: Int): List[Person] =
      {
        if (pending == 0) tmp
        else {
          val p = new Person(pending)
          p.status = if ((randomBelow(100) + 1) == 1) Infected else Healthy
          populate(p :: tmp, pending - 1)
        }
      }

    populate(Nil, size)
  }

}
