package platform

class CharEvent(val codepoint: Int) extends AnyVal {
  override def toString: String = new String(Character.toChars(codepoint))
}
