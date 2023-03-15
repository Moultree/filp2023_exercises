package exercises03.game

import scala.math.Ordered.orderingToOrdered
import scala.util.{Failure, Success, Try}
object Game {
  def parseState(input: String, number: Int): State = {
    input.toIntOption match {
      case Some(i) =>
        i match {
          case a if a < number => NumberIsBigger
          case a if a > number => NumberIsSmaller
          case _               => Guessed
        }
      case None =>
        input match {
          case GameController.IGiveUp => GiveUp
          case _                      => WrongInput
        }
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
    state == GiveUp || state == Guessed
  }
}
