package kamin

trait NameTokenizer {
  def isName(s: String) : Boolean = s.forall(c => c.isLetter || isAllowedSpecialChar(c))

  private def isAllowedSpecialChar(c: Char): Boolean =
    !Set(';', '(', ')').contains(c) && !c.isWhitespace
}
