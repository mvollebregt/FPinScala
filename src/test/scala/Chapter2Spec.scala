import org.specs2.mutable._

import Chapter2._

class Chapter2Spec extends Specification {

  // exercise 1
  "Fibonacci" should {
    "return 0 for fib(1)" in {
      fib(1) must_== 0
    }
    "return 1 for fib(2)" in {
      fib(2) must_== 1
    }
    "return 2 for fib(4)" in {
      fib(4) must_== 2
    }
    "return 21 for fib(9)" in {
      fib(9) must_== 21
    }
  }

  // exercise 2
  "isSorted" should {
    "return true for an empty array" in {
      isSorted(Array[Int](), (x : Int, y : Int) => x < y) must_== true
    }
    "return true for a sorted array" in {
      isSorted(Array('c', 'b', 'a'), (x: Char, y: Char) => x > y) must_== true
    }
    "return false for an unsorted array" in {
      isSorted(Array(3.4, 3.6, 3.5), (_:Double) <= (_:Double)) must_== false
    }
  }

  // exercise 3
  "curry" should {
    "make a curried version of the same function" in {
      def function(a: Int, b: Int) : Int = a + b
      def curried = curry(function)
      function(3, 4) must_== curried(3)(4)
    }
  }

  // exercise 4
  "uncurry" should {
    "make an uncurried version of the same function" in {
      def function(a: Int)(b: Int) : Int = a + b
      def uncurried = uncurry(function)
      function(3)(4) must_== uncurried(3, 4)
    }
  }

  // exercise 5
  "compose" should {
    "combine two functions into one" in {
      def f(b: Int) = b + 1
      def g(a: Int) = a * 2
      compose(f, g)(3) must_== f(g(3))
    }
  }
}