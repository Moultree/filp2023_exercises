package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию calculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Mul(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(l), Success(r)) => Success(l * r)
        case _ => DivisionByZero
      }
    case Div(left, right) =>
      (calculate(left), calculate(right)) match {
        case (_, Success(r)) if isZero(r) => DivisionByZero
        case (Success(l), Success(r)) => Success(l / r)
        case _ => DivisionByZero
      }
    case Plus(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(l), Success(r)) => Success(l + r)
        case _ => DivisionByZero
      }
    case Minus(left, right) =>
      (calculate(left), calculate(right)) match {
        case (Success(l), Success(r)) => Success(l - r)
        case _ => DivisionByZero
      }
    case Val(v) => Success(v)
    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(c) =>
          if (iff(c)) calculate(left)
          else calculate(right)
        case _ => DivisionByZero
      }
  }
}
