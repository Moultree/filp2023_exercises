package exercises03

object SetFunctions {
  type Set[A] = A => Boolean

  def contains[A](s: Set[A], elem: A): Boolean = s(elem)

  def singletonSet[A](elem: A): Set[A] = x => x == elem

  def union[A](s: Set[A], t: Set[A]): Set[A] = x => s(x) || t(x)

  def intersect[A](s: Set[A], t: Set[A]): Set[A] = x => s(x) && t(x)

  def diff[A](s: Set[A], t: Set[A]): Set[A] = x => s(x) && !t(x)

  def symmetricDiff[A](s: Set[A], t: Set[A]): Set[A] = diff(union(s, t), intersect(s, t))

  def filter[A](s: Set[A], p: A => Boolean): Set[A] = x => s(x) && p(x)

  def cartesianProduct[A, B](as: Set[A], bs: Set[B]): Set[(A, B)] =
    x => as(x._1) && bs(x._2)
}
