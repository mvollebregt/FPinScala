
trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case MySome(a) => MySome(f(a))
    case MyNone => MyNone
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this map f getOrElse MyNone

  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(a) => a
    case MyNone => default
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this map (x => MySome(x)) getOrElse ob

  def filter(f: A => Boolean): MyOption[A] = this flatMap ((x: A) => if (f(x)) MySome(x) else MyNone)
}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]
