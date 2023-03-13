package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = {
    case arg =>
      funcs.view.map(_.lift(arg)).collectFirst { case Some(result) => result }
  }
}
