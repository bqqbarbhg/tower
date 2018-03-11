package input

import InputSet._
import core.Identifier

object InputSet {
  abstract class Element
  case class Button(name: Identifier) extends Element
}

class InputSet(val name: String) {

  def button(buttonName: String): Button = Button(Identifier(name + "." + buttonName))

}

