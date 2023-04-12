package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object Eq {}

object EqInstances {
  implicit val intEq: Eq[Int] = _ == _

  implicit val boolEq: Eq[Boolean] = _ == _

  implicit def eqOption[A](implicit eqInst: Eq[A]): Eq[Option[A]] = new Eq[Option[A]] {
    override def eqv(a: Option[A], b: Option[A]): Boolean =
      (a, b) match {
        case (Some(aVal), Some(bVal)) => eqInst.eqv(aVal, bVal)
        case (None, None)             => true
        case _                        => false
      }
  }

  implicit def eqList[A](implicit eqInst: Eq[A]): Eq[List[A]] = new Eq[List[A]] {
    override def eqv(a: List[A], b: List[A]): Boolean = a.length == b.length && a.corresponds(b)(eqInst.eqv)
  }

  implicit val listBoolEq: Eq[List[Boolean]] = _ == _
}

object EqSyntax {
  implicit class EqOps[A](val a: A) extends AnyVal {
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
  // 1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
}
