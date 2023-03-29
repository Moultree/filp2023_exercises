package exercises05.parser

import exercises05.either.EitherCombinators._
import Error._

import scala.util.matching.Regex

object Examples {
  private val passReg = new Regex("(\\d{4}) (\\d{6})")

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть None
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть None
    * если rawUser.id не парсится в Long то функция должна вернуть None
    * если rawUser.banned, то вернуть None
    * используйте for-comprehension
    */
  def transformToOption(rawUser: RawUser): Option[User] =
    for {
      firstName  <- rawUser.firstName
      secondName <- rawUser.secondName
      passport   <- toOptionPassport(rawUser.passport)
      id         <- rawUser.id.toLongOption
      if !rawUser.banned
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)

  private def toOptionPassport(passport: Option[String]): Option[Option[Passport]] =
    passport match {
      case None => Some(None)
      case Some(v1) =>
        passReg.findFirstMatchIn(v1) match {
          case None => None
          case Some(v2) =>
            (v2.group(1).toLongOption, v2.group(2).toLongOption) match {
              case (_, None) | (None, _) => None
              case (Some(s), Some(num))  => Some(Some(Passport(s, num)))
            }
        }
    }

  /**
    * если rawUser.firstName или rawUser.secondName == None, то функция должна вернуть Left(InvalidName)
    * если rawUser.passport == None или rawUser.thirdName == None, то и в результирующем User эти поля == None
    * passport должен быть передан в формате 1234 567890, если не так, то функция должна вернуть Left(InvalidPassport)
    * если rawUser.id не парсится в Long то функция должна вернуть Left(InvalidId)
    * если rawUser.banned, то вернуть Left(Banned)
    * у ошибок есть приоритет:
    * 1. Banned
    * 2. InvalidId
    * 3. InvalidName
    * 4. InvalidPassport
    * используйте for-comprehension
    * но для того, чтобы for-comprehension заработал надо реализовать map и flatMap в Either
    */
  def transformToEither(rawUser: RawUser): Either[Error, User] =
    for {
      _          <- if (rawUser.banned) Left(Banned) else Right(true)
      id         <- Either.fromOption(rawUser.id.toLongOption)(InvalidId)
      firstName  <- Either.fromOption(rawUser.firstName)(InvalidName)
      secondName <- Either.fromOption(rawUser.secondName)(InvalidName)
      passport   <- Either.fromOption(toOptionPassport(rawUser.passport))(InvalidPassport)
    } yield User(id, UserName(firstName, secondName, rawUser.thirdName), passport)

}
