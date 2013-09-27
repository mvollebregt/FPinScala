import scala.annotation.tailrec

package fpinscala.datastructure {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

}

object Chapter3Tree {

  import fpinscala.datastructure._

//  def sizee(t: Tree[_]) : Int = {
//    t match {
//      case Leaf(_) => 1
//      case Branch(a, b) => 1 + sizee(a) + sizee(b) // not tail recursive, nor very efficient
//    }
//  }
//
//  def maximum(t: Tree[Int]) : Int = {
//    t match {
//      case Leaf(i) => i
//      case Branch(a, b) => maximum(a) max maximum(b)
//    }
//  }
//
//  def depth(t: Tree[_]): Int = {
//    t match {
//      case Leaf(i) => 1
//      case Branch(a, b) => 1 + (depth(a) max depth(b))
//    }
//  }
//
//  def mapp[T](t: Tree[T])(f: T => T): Tree[T] = {
//    t match {
//      case Leaf(i) => Leaf(f(i))
//      case Branch(a, b) => Branch(mapp(a)(f), mapp(b)(f))
//    }
//  }

  def fold[T, A](t: Tree[T])(f_leaf: T => A, f_acc: (A, A) => A) : A = {
    t match {
      case Leaf(i) => f_leaf(i)
      case Branch(a, b) => f_acc(fold(a)(f_leaf, f_acc), fold(b)(f_leaf, f_acc))
    }
  }

  def sizee[T](t: Tree[T]) : Int = fold[T, Int](t)(_ => 1, (a, b) => 1 + a + b)

  def maximum(t: Tree[Int]) : Int = fold[Int, Int](t)(x => x, (a, b) => a max b)

  def depth[T](t: Tree[T]): Int = fold[T, Int](t)(_ => 1, (a, b) => 1 + (a max b))

  def mapp[T](t: Tree[T])(f: T => T) = fold[T, Tree[T]](t)(x => Leaf(f(x)), (a, b) => Branch(a, b))


}