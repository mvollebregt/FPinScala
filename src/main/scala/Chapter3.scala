import scala.annotation.tailrec

package fpinscala.datastructure {

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
}

}

object Chapter3 {

  import fpinscala.datastructure._
  import fpinscala.datastructure.List._

  // exercise 1
  def matchQuestion = 3

  // exercise 2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil // or throw exception?
    case Cons(_, xs) => xs
  }

  // exercise 3
  def setHead[A](l: List[A], y: A): List[A] = l match {
    case Nil => Cons(y, Nil) // or throw exception?
    case Cons(_, xs) => Cons(y, xs)
  }

  // exercise 4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (0, l) => l
    case (n, Nil) if n >= 1 => Nil
    case (n, Cons(x, xs)) if n >= 1 => drop(xs, n - 1)
  }

  // exercise 5
  @tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (!f(x)) l else dropWhile(xs)(f)
  }

  //  def reverse[A](l: List[A]) : List[A] =  {
  //    @tailrec
  //    def go(acc: List[A], rest: List[A]) : List[A] = rest match {
  //      case Nil => acc
  //      case Cons(x, xs) => go(Cons(x, acc), xs)
  //    }
  //    go(Nil, l)
  //  }

  // exercise 6
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def go(acc: List[A], rest: List[A]): List[A] = rest match {
      case Nil => acc
      case Cons(_, Nil) => acc
      case Cons(x, xs) => go(Cons(x, acc), xs)
    }
    reverse(go(Nil, l))
  }

  // exercise 8
  def foldRightQuestion = Cons(1, Cons(2, Cons(3, Nil)))

  // exercise 9
  def lengthh[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  // exercise 10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // exercise 11
  def foldLeftSum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def foldLeftProduct(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def foldLeftLength(l: List[_]): Int = foldLeft(l, 0)((n, _) => n + 1)

  // exercise 12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((l, a) => Cons(a, l))

  // exercise 13
  def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a: A, g: B => B) => ((b: B) => g(f(b, a))))(z)

  def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((a, b) => f(b, a))

  // exercise 14
  def append[A](xs: List[A], ys: List[A]): List[A] = foldRightL(xs, ys)(Cons(_, _))

  // exercise 15
  def flatten[A](xs: List[List[A]]): List[A] = foldLeft(xs, List[A]())(append)

  // exercise 16
  def addOneToEachElement(xs: List[Int]): List[Int] = foldRightL(xs, List[Int]())((a, b) => Cons(a + 1, b))

  // exercise 17
  def eachElementToString(xs: List[Double]): List[String] = foldRightL(xs, List[String]())((a, b) => Cons(a.toString, b))

  // exercise 18
  def mapp[A, B](l: List[A])(f: A => B): List[B] = foldRightL(l, List[B]())((a, b) => Cons(f(a), b))

  // exercise 19
  def filter[A](l: List[A])(p: A => Boolean): List[A] = foldRightL(l, List[A]())((a, b) => if (p(a)) Cons(a, b) else b)

  // exercise 20
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(mapp(l)(f))

  // exercise 21
  def filterFM[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(x => if (p(x)) List(x) else Nil)

  // exercise 22
  def addCorresponding(xs: List[Int])(ys: List[Int]): List[Int] = {
    @tailrec
    def go(acc: List[Int], rxs: List[Int], rys: List[Int]): List[Int] =
      (rxs, rys) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => go(Cons(x + y, acc), xs, ys)
      }
    reverse(go(List[Int](), xs, ys))
  }

  // exercise 23
  def combine[A](xs: List[A])(ys: List[A])(f: (A, A) => A): List[A] = {
    @tailrec
    def go(acc: List[A], rxs: List[A], rys: List[A]): List[A] =
      (rxs, rys) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(x, xs), Cons(y, ys)) => go(Cons(f(x, y), acc), xs, ys)
      }
    reverse(go(List[A](), xs, ys))
  }

  // exercise 24
  def hasSubsequence[A](l : List[A], sub: List[A]) : Boolean = {
    // for each of the remaining subs:
    // - checks if the first element of the sub equals x
    // - if so, adds the remainder of the sub to acc
    // - if the remainder is nil, sets foundMatch to true
    @tailrec
    def filterSubs(x: A, remainingSubs: List[List[A]], acc: List[List[A]] = Nil, foundMatch : Boolean = false) : (List[List[A]], Boolean) =
      remainingSubs match {
        case Nil => (acc, foundMatch)
        case Cons(Cons(y, ys), yss) if (x == y) => filterSubs(x, yss, Cons(ys, acc), foundMatch || (ys == Nil))
        case Cons(Cons(y, _), yss) if (x != y) => filterSubs(x, yss, acc, foundMatch)
      }

    @tailrec
    def go(remainingList: List[A], remainingSubs: List[List[A]], sub: List[A]) : Boolean = {
      (remainingList, remainingSubs) match {
        case (Nil, _) => false
        case (Cons(x, xs), yss) =>
          filterSubs(x, remainingSubs) match {
            case (_, true) => true
            case (newRemainingSubs, false) => go(xs, Cons(sub, newRemainingSubs), sub)
          }
      }
    }

    (sub == Nil) || go(l, Cons(sub, Nil), sub)
  }

}
