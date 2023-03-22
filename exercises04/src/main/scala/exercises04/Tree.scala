package exercises04

import scala.annotation.tailrec

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Необходимо реализовать операции на бинарном дереве
object Tree {
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @tailrec
    def loop(q: List[Tree[A]], acc: Option[B]): B = {
      (q, acc) match {
        case (Leaf(a) :: tail, None)          => loop(tail, Some(f(a)))
        case (Leaf(a) :: tail, Some(b))       => loop(tail, Some(g(b, f(a))))
        case (Branch(left, right) :: tail, _) => loop(left :: right :: tail, acc)
        case (Nil, Some(b))                   => b
      }
    }
    loop(List(t), None)
  }

  def size[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def max(t: Tree[Int]): Int = fold(t)(identity)(math.max)

  def depth[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => math.max(a, b) + 1)

  // тут может пригодиться явное указание типа
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
