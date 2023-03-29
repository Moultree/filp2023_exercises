package exercises05
import scala.collection.mutable

object Combinators {
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
