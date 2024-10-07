package kamin

trait IntegerTokenizer {
  def isInteger(s: String) : Boolean = s.head == '-' && s.tail.forall(_.isDigit) || s.forall(_.isDigit)
}
