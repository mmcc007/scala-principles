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

//  lazy val genHeap: Gen[H] = ???
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf(empty, genHeap)
  } yield insert(x, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  property("minimum of 2 inserted to heap") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == a.min(b)
  }

  property("deleteMin of two element inserted to heap") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    deleteMin(h) == insert(a.max(b), empty)
  }

  property("meld 2 heaps and get minimum") = forAll { (h1: H, h2: H) =>
    val min = findMin(h1) min findMin(h2)
    findMin(meld(h1, h2)) == min
  }

  property("transferring the minimum from one heap to another heap") = forAll { (h1: H, h2: H) =>
    def reduce(h: H): List[A] =
      if (!isEmpty(h)) findMin(h) :: reduce(deleteMin(h))
      else Nil
    val melded = meld(h1, h2)
    val transferred = meld(deleteMin(h1), insert(findMin(h1), h2))
    reduce(melded) == reduce(transferred)
  }

}
