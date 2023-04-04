package exercises05
import scala.collection.mutable

object Combinators {
  // There is a chain of hefEgGeGFEgGgeHE
  // Different types of particles are present in this chain:
  // f, e, h, and g are positively charged particles
  // F, E, H, and G are negatively charged particles
  // If particles of the same type but opposite polarity are next to each other in the chain, they react and disappear
  // The process goes from left to right
  //
  // hefEgGeGFEgGgeHE <- gG reacted
  // hefEeGFEgGgeHE <- Ee reacted
  // hefGFEgGgeHE <- gG reacted
  // hefGFEgeHE <- the resulting chain, with 10 particles
  //
  // Write a function that uses standard library combinators
  // to fully react the chain
  def react(ipt: String): String = {
    val reacted = ipt
      .foldLeft(new StringBuilder()) {
        case (sb, c) if sb.nonEmpty && sb.last != c && sb.last.toLower == c.toLower =>
          sb.deleteCharAt(sb.length - 1)
        case (sb, c) => sb.append(c)
      }
    reacted.mkString
  }
}
