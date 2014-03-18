// chop off the mandatory "return" at the end of for-comprehension
// aid in monadic programming
object NoReturn {
  def map[T](f: Unit => T): T = f(())
}
