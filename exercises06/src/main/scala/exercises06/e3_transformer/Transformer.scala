package exercises06.e3_transformer
import exercises06.e3_transformer.Error.{InvalidId, InvalidName}

trait Transformer[A, B] {
  def toOption(a: A): Option[B]

  def toEither(a: A): Either[Error, B]
}

object TransformerInstances {
  implicit val transformer: Transformer[RawUser, User] = new Transformer[RawUser, User] {
    override def toOption(a: RawUser): Option[User] = a.id.toLongOption match {
      case Some(id) if a.firstName.exists(_.nonEmpty) && a.secondName.exists(_.nonEmpty) =>
        val firstName  = a.firstName.getOrElse("")
        val secondName = a.secondName.getOrElse("")
        val thirdName  = a.thirdName.filter(_.nonEmpty)
        Some(User(id, UserName(firstName, secondName, thirdName)))
      case _ =>
        None
    }

    override def toEither(a: RawUser): Either[Error, User] = {

      def nonEmptyString(s: Option[String], err: Error): Either[Error, String] =
        s.filter(_.nonEmpty).toRight(err)

      for {
        id         <- a.id.toLongOption.toRight(InvalidId)
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
