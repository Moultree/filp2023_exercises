package exercises05

/**
  * Необходимо реализовать prepend, append, withFilter и toList
  * Метод prepend принимает на вход список s, добавляемый в начало
  * Вернуть надо новый объект, сконструировав его от новой функции
  * Эта функция должна возвращать список s , добавленный в начало к списку, возвращаемому из calculate
  * Метод append принимает на вход список s, добавляемый в конец.
  * Вернуть надо новый объект, сконструировав его от новой функции.
  * Эта функция должна возвращать результат применения функции calculate к конкатенации списка s и аргумента этой функции.
  * withFilter должна проверить предикат по всем элементам списка
  * Метод toList применяет все накопленные операции и отдаёт итоговый список.
  */
final class DiffList[A](calculate: List[A] => List[A]) {
  def prepend(s: List[A]): DiffList[A] =
    new DiffList((xs: List[A]) => s ++ calculate(xs))

  def append(s: List[A]): DiffList[A] =
    new DiffList((xs: List[A]) => calculate(xs ++ s))

  def withFilter(f: A => Boolean): DiffList[A] =
    new DiffList((xs: List[A]) => calculate(xs).filter(f))

  def toList: List[A] = calculate(Nil)
}

object DiffList {
  def apply[A]: DiffList[A] =
    new DiffList(xs => xs)

  def singletonList[A](el: A): DiffList[A] =
    new DiffList(xs => el :: xs)
}