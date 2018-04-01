package menu.gui

import core._
import ui.Canvas.TextStyle
import ui.InputSet.InputArea
import ui._

import scala.util.Try

case class SliderStyle(height: Double, labelWidth: Double, labelPad: Double, rectSprite: Identifier, stringFormat: Double => String, stringParse: String => Option[Double], lineWidth: Double, markWidth: Double)

abstract class Slider(val style: SliderStyle, val textBoxStyle: TextBoxStyle) extends Element {
  def minValue: Double
  def maxValue: Double
  def currentValue: Double
  def clamped: Boolean = false
  def setValue(newValue: Double): Unit
  def step: Option[Double]

  val sliderInput = new InputArea()
  var lastLayout: Layout = Layout.Empty

  private def setValueImpl(v: Double): Unit = {
    if (clamped)
      setValue(clamp(v, minValue, maxValue))
    else
      setValue(v)
  }

  val textBox = new TextBox(textBoxStyle) {
    override def currentText: String = Slider.this.style.stringFormat(currentValue)
    override def setText(newValue: String): Unit = {
      val maybeV = Slider.this.style.stringParse(newValue)
      for (v <- maybeV) {
        setValueImpl(v)
      }
    }

    override def enabled: Boolean = Slider.this.enabled
  }

  def update(parent: Layout): Unit = {
    val fullRect = parent.pushTop(style.height)
    lastLayout = fullRect.copy
    val rect = fullRect.copy
    val label = rect.pushRight(style.labelWidth).padLeft(style.labelPad)
    val base = rect.position
    val size = rect.size
    val min = minValue
    val max = maxValue

    val unit = parent.unit
    val mw = style.markWidth * unit.y
    val mh = size.y * 0.6
    val sliderWidth = size.x - mw

    if (enabled && sliderInput.dragged) {
      val pos = inputs.dragPosition
      val relX = clamp((pos.x - base.x) / sliderWidth, 0.0, 1.0)
      val value = min * (1.0 - relX) + max * relX
      setValueImpl(value)
    }

    if (visible) {

      val value = currentValue

      if (max > min + 0.0001) {

        val s = style.rectSprite
        val lw = style.lineWidth * unit.y
        val lh = size.y * 0.5
        val lineCol = if (enabled) {
          Color(0.0, 0.0, 0.0, 0.4)
        } else {
          Color(0.0, 0.0, 0.0, 0.2)
        }

        val markCol = if (sliderInput.focused || sliderInput.dragged) {
          Color(1.0, 1.0, 1.0, 1.0)
        } else if (enabled) {
          Color(0.7, 0.7, 0.7, 1.0)
        } else {
          Color(0.2, 0.2, 0.2, 1.0)
        }

        canvas.draw(0, s, base + Vector2(lw, -lw * 0.5 + size.y * 0.5), Vector2(size.x - 2.0 * lw, lw), lineCol)
        canvas.draw(0, s, base + Vector2(0.0, size.y * 0.5 - lh * 0.5), Vector2(lw, lh), lineCol)
        canvas.draw(0, s, base + Vector2(size.x - lw, size.y * 0.5 - lh * 0.5), Vector2(lw, lh), lineCol)

        val rel = clamp((value - min) / (max - min), 0.0, 1.0)

        canvas.draw(0, s, base + Vector2(sliderWidth * rel, size.y * 0.5 - mh * 0.5), Vector2(mw, mh), markCol)

        if (enabled)
          inputs.add(0, sliderInput, rect, 0.0, 0)
      }

      textBox.inputs = inputs
      textBox.canvas = canvas
      textBox.update(label)
    }
  }
}


