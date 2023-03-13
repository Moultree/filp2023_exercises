package exercises03.game

import scala.util.{Failure, Success, Try}
object Game {
  def parseState(input: String, number: Int): State = {
    Try(input.toInt) match {
      case Failure(_) if input.equalsIgnoreCase(GameController.IGiveUp) => GiveUp
      case Failure(_)                                                   => WrongInput
      case Success(guess) if guess < number                             => NumberIsBigger
      case Success(guess) if guess > number                             => NumberIsSmaller
      case Success(guess) if guess == number                            => Guessed
    }
  }

  def action(state: State, number: Int): GameController => Unit = {
    state match {
      case GiveUp          => _.giveUp(number)
      case WrongInput      => _.wrongInput()
      case NumberIsBigger  => _.numberIsBigger()
      case NumberIsSmaller => _.numberIsSmaller()
      case Guessed         => _.guessed()
    }
  }

  def completed(state: State): Boolean = {
    state match {
      case GiveUp | Guessed => true
      case WrongInput       => false
      case _                => false
    }
  }
}
