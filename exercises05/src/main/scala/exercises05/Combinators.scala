package exercises05
import scala.collection.mutable

object Combinators {
  // Есть цепочка hefEgGeGFEgGgeHE
  // в данной цепочке есть различные типы частиц
  // f, e, h, g положительно заряженные частицы
  // F, E, H, G отрицательно заряженные частицы
  // если частицы одного типа с разной полярностью стоят вместе в цепочке, они реагируют и исчезают
  // проход слева направо
  //
  // hefEgGeGFEgGgeHE <- gG прореагировали
  // hefEeGFEgGgeHE <- Ee прореагировали
  // hefGFEgGgeHE <- gG
  // hefGFEgeHE <- итоговая цепочка, в которой 10 частиц
  //
  // Напишите функцию, используя комбинаторы стандартной библиотеки,
  // которая проведёт полную реакцию
  def react(ipt: String): String = {
    val q     = mutable.Queue[Char](ipt: _*)
    val stack = mutable.Stack[Char]()

    while (q.nonEmpty) {
      val c = q.dequeue()
      if (stack.nonEmpty && stack.top != c && stack.top.toLower == c.toLower) {
        stack.pop()
      } else {
        stack.push(c)
      }
    }
    stack.reverse.mkString
  }
}
