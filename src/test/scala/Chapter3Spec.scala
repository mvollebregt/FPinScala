import org.specs2.mutable._

import Chapter3._

class Chapter3Spec extends Specification {

  import fpinscala.datastructure._
  import fpinscala.datastructure.List._

  // exercise 1
  "matchExample" should {
    "return the correct answer to the question" in {
      val x: Int = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
      x must_== matchQuestion
    }
  }

  // exercise 2
  "tail" should {
    "return the remaining elements" in {
      tail(List(1, 2, 3)) must_== List(2, 3)
    }
    "return nil for nil?" in {
      tail(Nil) must_== Nil
    }
  }

  // exercise 3
  "setHead" should {
    "change the first element" in {
      setHead(List(1, 2, 3), 4) must_== List(4, 2, 3)
    }
    "return a single element list for nil?" in {
      setHead(Nil, 'x') must_== List('x')
    }
  }

  // exercise 4
  "drop" should {
    "drop the first n elements" in {
      drop(List(5, 6, 7, 8, 9), 3) must_== List(8, 9)
    }
    "return nil if list is nil" in {
      drop(Nil, 0) must_== Nil
    }
    "return nil if list is too short" in {
      drop(List(2), 2) must_== Nil
    }
    "return nil if list is exact length" in {
      drop(List(1, 2), 2) must_== Nil
    }
  }

  // exercise 5
  "dropWhile" should {
    "drop elements" in {
      dropWhile(List(5, 6, 7, 8, 9))(_ <= 7) must_== List(8, 9)
    }
    "return nil if list is nil" in {
      dropWhile(Nil)((_: Int) <= 7) must_== Nil
    }
    "return nil if all is dropped" in {
      dropWhile(List(5, 6, 7))(_ <= 7) must_== Nil
    }
  }

  // exercise 6
  "init" should {
    "return all but last element" in {
      init(List(5, 6, 7, 8, 9)) must_== List(5, 6, 7, 8)
    }
    "return an empty list for a single element list" in {
      init(List(6)) must_== Nil
    }
  }

  // exercise 8
  "foldRightQuestion" should {
    "return the correct answer" in {
      foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) must_== foldRightQuestion
    }
  }

  // exercise 9
  "lengthh" should {
    "return the length of the list" in {
      lengthh(List('g', 'e', 'z', 'g')) must_== 4
    }
    "return 0 for an empty list" in {
      lengthh(Nil) must_== 0
    }
  }

  // exercise 10
  "foldLeft" should {
    "fold left" in {
      foldLeft(List(2, 3, 4), 0)(_ - _) must_== -9
    }
  }

  // exercise 11
  "sum, product and lengthh" should {
    "calculate sum" in {
      foldLeftSum(List(2, 4, 8)) must_== 14
    }
    "calculate product" in {
      foldLeftProduct(List(2, 4, 8)) must_== 64
    }
    "calculate length" in {
      foldLeftLength(List('a', 'r', 'p')) must_== 3
    }
  }

  // exercise 12
  "reverse" should {
    "reverse the list" in {
      reverse(List(3, 5, 7)) must_== List(7, 5, 3)
    }
  }

  // exercise 13
  "foldLeftR and foldRightL" should {
    "do the same thing as foldLeft" in {
      foldLeftR(List(2, 3, 4), 0)(_ - _) must_== foldLeft(List(2, 3, 4), 0)(_ - _)
    }
    "do the same thing as foldRight" in {
      foldRightL(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) must_== foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    }
  }

  // exercise 14
  "append" should {
    "append two lists" in {
      append(List(1, 2), List(3, 4)) must_== List(1, 2, 3, 4)
    }
  }

  // exercise 15
  "flatten" should {
    "flatten lists" in {
      flatten(List(List(1, 2), List(3, 4, 5), List(6))) must_== List(1, 2, 3, 4, 5, 6)
    }
  }

  // exercise 16
  "addOneToEachElement" should {
    "add one to each element" in {
      addOneToEachElement(List(4, 6, 2, 1)) must_== List(5, 7, 3, 2)
    }
  }

  // exercise 17
  "eachElementToString" should {
    "turn each element into a String" in {
      eachElementToString(List(3.4, 5.7, 1.0)) must_== List("3.4", "5.7", "1.0")
    }
  }

  // exercise 18
  "map" should {
    "apply a function to each element in a list" in {
      mapp(List(4, 6, 2, 1))(_ + 1) must_== List(5, 7, 3, 2)
    }
  }

  // exercise 19
  "filter" should {
    "remove all elements that do not satisfy a condition" in {
      filter(List(1, 2, 3, 5, 6, 7))(_ % 2 == 0) must_== List(2, 6)
    }
  }

  // exercise 20
  "flatMap" should {
    "map a result and flatten it into a list" in {
      flatMap(List(1, 2, 3))(i => List(i, i)) must_== List(1, 1, 2, 2, 3, 3)
    }
  }

  // exercise 21
  "filterFM" should {
    "remove all elements that do not satisfy a condition" in {
      filterFM(List(1, 2, 3, 5, 6, 7))(_ % 2 == 0) must_== List(2, 6)
    }
  }

  // exercise 22
  "addCorresponding" should {
    "add corresponding elements" in {
      addCorresponding(List(1, 2, 3))(List(4, 5, 6)) must_== List(5, 7, 9)
    }
  }

  // exercise 23
  "combine" should {
    "combine corresponding elements" in {
      combine(List(1, 2, 3))(List(4, 5, 6))(_ + _) must_== List(5, 7, 9)
    }
  }

  // exercise 24
  "hasSubsequence" should {
    "determine if the second list is a subsequence of the first list" in {
      hasSubsequence(List(1, 2, 3, 4), List(2, 3)) must_== true
    }
    "return false if this is not the case" in {
      hasSubsequence(List(1, 2, 3, 4), List(3, 4, 5)) must_== false
    }
    "return true if this is the case" in {
      hasSubsequence(List(1, 2, 1, 2, 3), List(1, 2, 3)) must_== true
    }
    "return true if this is the case" in {
      hasSubsequence(List(1, 1, 1, 2), List(1, 1, 2)) must_== true
    }
    "return true if subsequence is empty" in {
      hasSubsequence(Nil ,Nil) must_== true
    }
    "return false if list is empty" in {
      hasSubsequence(Nil, List(1)) must_== false
    }
  }
}