package exercises06.e3_transformer

import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    override def toOption(a: RawUser): Option[User] = toEither(a).toOption

    def fromOption[A, B](option: Option[B])(a: => A): Either[A, B] = {
      option match {
        case None    => Left[A, B](a)
        case Some(b) => Right(b)
      }
    }

    override def toEither(a: RawUser): Either[Error, User] = {

      def nonEmptyString(s: Option[String], err: Error): Either[Error, String] =
        s.filter(_.nonEmpty).toRight(err)

      for {
        id         <- fromOption(a.id.toLongOption)(Error.InvalidId)
        firstName  <- nonEmptyString(a.firstName, InvalidName)
        secondName <- nonEmptyString(a.secondName, InvalidName)
        thirdName  <- Right(a.thirdName.filter(_.nonEmpty))
      } yield User(id, UserName(firstName, secondName, thirdName))
    }
  }
}

object TransformerSyntax {
  implicit class TransformerOps[A](a: A) {
    def transformToOption[B](implicit transformer: Transformer[A, B]): Option[B] =
      transformer.toOption(a)

    def transformToEither[B](implicit transformer: Transformer[A, B]): Either[Error, B] =
      transformer.toEither(a)
  }
}

object Examples {
  import TransformerInstances._
  import TransformerSyntax._

  RawUser("1234", Some(""), Some(""), None).transformToOption[User]
  RawUser("1234", Some(""), Some(""), None).transformToEither[User]
}
