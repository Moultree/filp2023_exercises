package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {
  implicit val intEq: Eq[Int] = new Eq[Int] {
    def eqv(a: Int, b: Int): Boolean = a == b
  }

  implicit val boolEq: Eq[Boolean] = new Eq[Boolean] {
    def eqv(a: Boolean, b: Boolean): Boolean = a == b
  }

  implicit def eqOption[A](implicit eqInst: Eq[A]): Eq[Option[A]] = new Eq[Option[A]] {
    override def eqv(a: Option[A], b: Option[A]): Boolean =
      (a, b) match {
        case (Some(aVal), Some(bVal)) => eqInst.eqv(aVal, bVal)
        case (None, None)             => true
        case _                        => false
      }
  }

  implicit def eqList[A](implicit eqInst: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
    override def eqv(a: List[A], b: List[A]): Boolean = {
      val aLen = a.length
      val bLen = b.length
      if (aLen == bLen) {
        val sameLen = a.zip(b).count(x => eqInst.eqv(x._1, x._2))
        if (sameLen == aLen) true else false
      } else false
    }
  }

  implicit val listIntEq: Eq[List[Int]] = new Eq[List[Int]] {
    def eqv(a: List[Int], b: List[Int]): Boolean = a.equals(b)
  }

  implicit val listBoolEq: Eq[List[Boolean]] = new Eq[List[Boolean]] {
    def eqv(a: List[Boolean], b: List[Boolean]): Boolean = a == b
  }

}

object EqSyntax {
  implicit class EqOps[A](val a: A) {
    def eqv(b: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, b)
    def ===(b: A)(implicit ev: Eq[A]): Boolean = eqv(b)
    def !==(b: A)(implicit ev: Eq[A]): Boolean = !eqv(b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  //1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
