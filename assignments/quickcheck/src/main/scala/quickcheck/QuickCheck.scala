package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("is_not_empty") = forAll { a: Int =>
    val h = insert(a, empty)
    !isEmpty(h)
  }

  property("min_after_two_insertions") = forAll(positiveInteger) { a: Int =>
    val h1 = insert(a, empty)
    val h2 = insert(a - 1, h1)
    findMin(h2) == a - 1
  }

  property("elements_linking") = forAll(positiveInteger) { a: Int =>
    val h1 = insert(2, empty)
    val h2 = insert(1, h1)
    val h3 = insert(0, h2)
    val h4 = insert(-1, h3)
    val h5 = insert(-2, h4)

    val min1 = findMin(h5)

    val h6 = deleteMin(h5)

    val min2 = findMin(h6)

    val h7 = deleteMin(h6)

    val min3 = findMin(h7)

    val h8 = deleteMin(h7)

    val min4 = findMin(h8)

    val h9 = deleteMin(h8)

    val min5 = findMin(h9)

    (min1 == -2) && (min2 == -1) && (min3 == 0) && (min4 == 1) && (min5 == 2)
  }

  property("size") = forAll { (h1: H, h2: H) =>

    val melded = meld(h1, h2)

    def size(n: Int, heap: H): Int = {
      isEmpty(heap) match {
        case true => n
        case false => size(n + 1, deleteMin(heap))
      }
    }

    size(0, melded) == (size(0, h1) + size(0, h2))
  }

  property("min_after_melded_with_empty") = forAll { h1: H =>

    val h2 = meld(h1, empty)

    findMin(h2) == findMin(h1)
  }

  property("min_after_deletion") = forAll { a: Int =>
    val h1 = insert(a, empty)
    isEmpty(deleteMin(h1))
  }

  property("meld") = forAll { (a: H, b: H) =>

    val c = meld(a, b)
    val min = findMin(c)

    (findMin(a) == min) || (findMin(b) == min)

  }

  def is_ordered(heap: H): Boolean = {

    isEmpty(heap) match {
      case false => {
        val min = findMin(heap)
        val rest = deleteMin(heap)

        isEmpty(rest) match {
          case false =>
            val next_min = findMin(rest)
            if (min <= next_min) is_ordered(rest) else false
          case _ => true
        }
      }
      case _ => true
    }
  }

  property("is_ordered") = forAll { a: H =>
    is_ordered(a)
  }

  property("is_ordered_after_melded") = forAll { (a: H, b: H) =>
    val melded = meld(a, b)
    is_ordered(melded)
  }

  lazy implicit val positiveInteger = Arbitrary.arbitrary[Int] suchThat (_ > 0)

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    b <- oneOf(empty, genHeap)
  } yield insert(a, b)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
