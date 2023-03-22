package exercises04

case class Machine(locked: Boolean, candies: Int, coins: Int)

/**
  * Реализуйте вендинговый аппарат по торговле барбарисками. Правила работы аппарата следующие:
  * если в закрытый аппарат вставляется монета (Coin), то аппартат открывается
  * если повернуть ручку (Turn) у открытого аппарата, то выйдет барбариска, и аппарат закроется
  * если в аппарате кончились барбариски, то он никак не реагирует. в этом случае надо вернуть список оставшихся Inputs и закончить
  * другие действия приводят к пропуску Input
  * если Input кончился, то заканчиваем
  * Подразумевается, что вы будете использовать паттерн-матчинг и рекурсию, так как while var isInstanceOf запрещены.
  */
object Machine {
  sealed trait Input
  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  @scala.annotation.tailrec
  def run(machine: Machine, inputs: List[Input]): (Machine, List[Input]) = {
    inputs match {
      case Nil => (machine, inputs)
      case input :: tail =>
        (machine, input) match {
          case (Machine(true, candies, coins), Input.Coin) =>
            run(Machine(locked = false, candies, coins + 1), tail)
          case (Machine(false, candies, coins), Input.Turn) if candies >= 1 =>
            run(Machine(locked = true, candies - 1, coins), tail)
          case (Machine(_, 0, _), _) => (machine, inputs)
          case _                     => run(machine, tail)
        }
    }
  }

}
