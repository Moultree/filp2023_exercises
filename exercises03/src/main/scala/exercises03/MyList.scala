package exercises03

import scala.annotation.tailrec

sealed trait MyList[+A]

final case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

case object Nil extends MyList[Nothing]

object MyList {
  def sum(list: MyList[Int]): Int = {
    @tailrec
    def innerSum(list: MyList[Int], acc: Int): Int = {
      list match {
        case Cons(head, tail) => innerSum(tail, acc + head)
        case Nil              => acc
      }
    }

    innerSum(list, 0)
  }

  def reverse[A](list: MyList[A]): MyList[A] = {
    def helper(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Nil              => acc
      case Cons(head, tail) => helper(tail, Cons(head, acc))
    }

    helper(list, Nil)
  }
}
