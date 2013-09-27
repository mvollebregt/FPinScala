import org.specs2.mutable._

class Chapter4Spec extends Specification {

  def fsome(x: Int) = MySome(x+1)
  def fnone(x: Int) = MyNone

  // exercise 1
  "map" should {
    "apply the function on MySome" in {
      MySome(12) map (_ + 1) must_== MySome(13)
    }
    "return MyNone on MyNone" in {
      (MyNone : MyOption[Int]) map (_ + 1) must_== MyNone
    }
  }
  "flatMap" should {
    "apply the function and flatten on MySome" in {
      MySome(12) flatMap fsome must_== MySome(13)
    }
    "return MyNone on MyNone" in {
      MyNone flatMap fsome must_== MyNone
    }
    "return MyNone if function returns MyNone" in {
      MySome(12) flatMap fnone must_== MyNone
    }
    "return MyNone if function returns MyNone on MyNone" in {
      MyNone flatMap fnone must_== MyNone
    }
  }
  "getOrElse" should {
    "return the embedded value in MySome" in {
      MySome(12) getOrElse (13) must_== 12
    }
    "return the alternative value in MyNone"in {
      MyNone getOrElse 13 must_== 13
    }
  }
  "orElse" should {
    "return the first value if defined" in {
      MySome(12) orElse MySome(13) must_== MySome(12)
    }
    "return the second value if not defined" in {
      MyNone orElse MySome(13) must_== MySome(13)
    }
  }
  "filter" should {
    "return option if true" in {
      MySome(12) filter (_ == 12) must_== MySome(12)
    }
    "return none if false" in {
      MySome(13) filter (_ == 12) must_== MyNone
    }
    "return none if none" in {
      MyNone filter (_ => true) must_== MyNone
    }
  }
}
