import org.specs2.mutable._

import Chapter3Tree._

class Chapter3TreeSpec extends Specification {

  import fpinscala.datastructure._

  // exercise 25
  "size" should {
    "return the size of a filled tree" in {
      sizee(Branch(Branch(Leaf('a'), Leaf('b')), Leaf('c'))) must_== 5
    }
  }

  "maximum" should {
    "return the max element of a filled tree" in {
      maximum(Branch(Branch(Leaf(3), Leaf(4)), Leaf(1))) must_== 4
    }
  }

  "depth" should {
    "return the depth of a filled tree" in {
      depth(Branch(Branch(Leaf(3), Leaf(4)), Leaf(1))) must_== 3
    }
  }

  "map" should {
    "apply a function to each element" in {
      mapp(Branch(Branch(Leaf(3), Leaf(4)), Leaf(1)))(_ + 1) must_== Branch(Branch(Leaf(4), Leaf(5)), Leaf(2))
    }
  }
}