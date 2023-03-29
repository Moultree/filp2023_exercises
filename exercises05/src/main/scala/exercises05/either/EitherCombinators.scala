package exercises05.either

object EitherCombinators {

  sealed trait Either[+A, +B] {
    def orElse[EE >: A, C >: B](other: => Either[EE, C]): Either[EE, C] = (this, other) match {
      case (_: Left[A, B], _: Right[A, B]) => other
      case _                               => this
    }

    def map2[AA >: A, BB, C](other: => Either[AA, BB])(f: (B, BB) => C): Either[AA, C] = (this, other) match {
      case (Right(b1), Right(b2)) => Right(f(b1, b2))
      case (Left(a), _)           => Left(a)
      case (_, Left(a))           => Left(a)
    }

    def map[BB](f: B => BB): Either[A, BB] = this match {
      case Right(v) => Right(f(v))
      case Left(v)  => Left(v)
    }

    def flatMap[AA >: A, BB](f: B => Either[AA, BB]): Either[AA, BB] = this match {
      case Right(b) => f(b)
      case Left(a)  => Left(a)
    }
  }

  case class Left[+A, +B](get: A) extends Either[A, B]

  case class Right[+A, +B](get: B) extends Either[A, B]

  object Either {
    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = option match {
      case None    => Left(a)
      case Some(v) => Right(v)
    }

    def traverse[E, A, B](list: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      list.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) => f(a).flatMap(b => acc.map(l => b :: l)))

    def sequence[E, A](list: List[Either[E, A]]): Either[E, List[A]] = traverse(list)(identity)
  }
}