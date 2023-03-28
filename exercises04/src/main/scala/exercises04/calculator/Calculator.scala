package exercises04.calculator

import scala.Integral.Implicits.infixIntegralOps

// Необходимо реализовать функцию calculate для вычисления выражений
class Calculator[T: Integral] {
  def isZero(t: T): Boolean =
    t == implicitly[Integral[T]].zero

  def div(l: Expr[T], r: Expr[T]): Result[T] = {
    calculate(r) match {
      case Success(v: T) =>
        v match {
          case 0 => DivisionByZero
          case _ =>
            calculate(l) match {
              case Success(w: T) => Success(w / v)
            }
        }
    }
  }
  def calculate(expr: Expr[T]): Result[T] = expr match {
    case Mul(left, right)   => applyBinOp(left, right, _ * _)
    case Div(left, right)   => div(left, right)
    case Plus(left, right)  => applyBinOp(left, right, _ + _)
    case Minus(left, right) => applyBinOp(left, right, _ - _)
    case Val(v)             => Success(v)
    case If(iff, cond, left, right) =>
      calculate(cond) match {
        case Success(c) => if (iff(c)) calculate(left) else calculate(right)
        case _          => DivisionByZero
      }
  }

  private def applyBinOp(left: Expr[T], right: Expr[T], op: (T, T) => T): Result[T] =
    (calculate(left), calculate(right)) match {
      case (Success(l), Success(r)) => Success(op(l, r))
      case _                        => DivisionByZero
    }
}
