package exercises02.game

class Game(controller: GameController) {

  /**
    * Игра угадай число
    * Ввод и вывод необходимо осуществлять с помощью методов controller
    *
    * Игра должна вызывать controller.askNumber перед каждой попыткой игрока угадать число
    * И вызвать controller.nextLine для получения ввода игрока
    * Если игрок ввел число меньше загаданного, игра должна вызвать controller.numberIsBigger
    * Если игрок ввел число больше загаданного, игра должна вызвать controller.numberIsSmaller
    * Если игрок угадал число, игра должна закончиться и вызвать controller.guessed
    * Если игрок написал GameController.IGiveUp, игра должна закончиться и вызвать controller.giveUp(number)
    * Если игрок ввел неизвестную комбинацию символов, надо вызвать controller.wrongInput и продолжить игру
    *
    * @param number загаданное число
    */
  def play(number: Int): Unit = {
    var guessed = false
    var input   = ""
    while (!guessed) {
      controller.askNumber()
      input = controller.nextLine()
      try {
        input.toInt match {
          case guess if guess < number => controller.numberIsBigger()
          case guess if guess > number => controller.numberIsSmaller()
          case guess if guess == number => {
            controller.guessed()
            guessed = true
          }
        }
      } catch {
        case _: NumberFormatException => {
          if (input == GameController.IGiveUp) {
            controller.giveUp(number)
            guessed = true
          } else {
            controller.wrongInput()
          }
        }
      }
    }
  }
}
