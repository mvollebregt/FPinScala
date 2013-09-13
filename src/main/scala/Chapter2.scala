import scala.annotation.tailrec
import scala.reflect.ClassTag

object Chapter2 {

  // exercise 1
  def fib(n : Int) : Int = {
    @tailrec
    def go(prev1: Int, prev2: Int, n: Int) : Int = {
      n match {
        case 1 => prev1
        case 2 => prev2
        case n => go(prev2, prev1 + prev2, n - 1)
      }
    }
    go(0, 1, n)
  }

  // exercise 2
  @tailrec
  def isSorted[A:ClassTag](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    as match {
      case Array() => true
      case Array(x) => true
      case Array(x1, x2, xs @ _*) => gt(x1, x2) && isSorted(Array(x2) ++ xs, gt)
    }
  }

  // exercise 3
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  // exercise 4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

}
