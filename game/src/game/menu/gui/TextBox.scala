package menu.gui

import core._
import ui._
import ui.Canvas.TextStyle
import ui.InputSet.InputArea
import TextBox._
import platform.{AppWindow, KeyEvent}

case class TextBoxStyle(height: Double, textStyle: TextStyle, selectedTextStyle: TextStyle,
                        idleBackgroundSprite: Identifier,
                        selectSprite: Identifier,
                        paddingLeft: Double,
                        yOffset: Double,
                        selectPad: Double,
                        caretWidth: Double,
                        doubleClickIntervalSeconds: Double,
                        selectAllOnFirstClick: Boolean,
                       )

object TextBox {
  case class Span(begin: Int, end: Int, pivot: Int)

  class EditContext {
    var text: String = ""
    var selection: Span = Span(0, 0, 0)
    var prevClickTime: Option[Double] = None
    var lastClickWasSelectAll: Boolean = false
    var brandNew: Boolean = true
  }
}

abstract class TextBox(val style: TextBoxStyle) extends Element {
  def currentText: String
  def setText(newValue: String): Unit

  var editContext: Option[EditContext] = None

  val input = new InputArea()

  def update(parent: Layout): Unit = {
    val unit = parent.unit
    val fullBox = parent.pushTop(style.height)
    val textBox = fullBox.copy.padLeft(style.paddingLeft)
    val pos = textBox.position + Vector2(0.0, style.yOffset)
    val baseTs = style.textStyle.scaled(parent.unit.y)
    val ts = if (enabled) {
      baseTs
    } else {
      baseTs.copy(color = baseTs.color * 0.5)
    }

    if (enabled) {

      def stopEdit(): Unit = {
        for (ec <- editContext) {
          setText(ec.text)
          editContext = None
        }
      }

      if (inputs.clicked) {
        if (input.clicked) {
          editContext match {
            case Some(ec) =>
            case None =>
              val ec = new EditContext()
              ec.text = currentText
              editContext = Some(ec)
          }
        } else {
          stopEdit()
        }
      }

      for (ec <- editContext) {
        def sb = ec.selection.begin
        def se = ec.selection.end
        def sp = ec.selection.pivot
        def tl = ec.text.length

        def replaceSelection(text: String): Unit = {
          ec.text = ec.text.take(sb) + text + ec.text.drop(se)
          val pos = sb + text.length
          ec.selection = Span(pos, pos, pos)
        }

        def findCharIndex(screenX: Double): Int = {
          val x = screenX - pos.x
          var ix = 0
          val font = ts.font.get
          var posX = 0.0
          while (ix < tl) {
            val ch = ec.text(ix)
            val next = if (ix + 1 < tl) ec.text(ix + 1) else '\0'
            val advance = font.getAdvance(ch, ts.height, next)
            val mid = posX + advance * 0.5
            if (mid > x) return ix
            posX += advance
            ix += 1
          }
          ix
        }

        def moveCaret(pos: Int, expandSelection: Boolean): Unit = {
          if (expandSelection) {
            if (pos < sp) {
              ec.selection = Span(pos, sp, sp)
            } else {
              ec.selection = Span(sp, pos, sp)
            }
          } else {
            ec.selection = Span(pos, pos, pos)
          }
        }

        if (input.clicked) {
          val time = AppWindow.currentTime
          val selAll = ec.prevClickTime.exists(_ + style.doubleClickIntervalSeconds >= time) || (ec.brandNew && style.selectAllOnFirstClick)
          ec.lastClickWasSelectAll = selAll
          if (selAll) {
            ec.selection = Span(0, tl, 0)
            ec.prevClickTime = None
          } else {
            val ix = findCharIndex(inputs.dragPosition.x)
            moveCaret(ix, AppWindow.modifierShiftDown)
            ec.prevClickTime = Some(time)
          }
          ec.brandNew = false
        }

        if (input.dragged && !input.clicked && !ec.lastClickWasSelectAll) {
          val ix = findCharIndex(inputs.dragPosition.x)
          moveCaret(ix, true)
        }

        def scanLeft(begin: Int, skipWhitespace: Boolean): Int = {
          var pos = math.max(begin - 1, 0)
          if (skipWhitespace && pos > 0) {
            val ws = ec.text(pos).isWhitespace
            while (pos > 0 && ec.text(pos - 1).isWhitespace == ws)
              pos -= 1
          }
          pos
        }

        def scanRight(begin: Int, skipWhitespace: Boolean): Int = {
          var pos = math.min(begin + 1, tl)
          if (skipWhitespace && pos < tl) {
            val ws = ec.text(pos).isWhitespace
            while (pos < tl && ec.text(pos).isWhitespace == ws)
              pos += 1
          }
          pos
        }

        for (key <- AppWindow.keyEvents.filter(_.down)) {

          if (key.control) {
            if (key.key == 'A') {
              ec.selection = Span(0, tl, 0)
            } else if (key.key == 'X' && se > sb) {
              AppWindow.setClipboard(ec.text.substring(sb, se))
              replaceSelection("")
            } else if (key.key == 'C' && se > sb) {
              AppWindow.setClipboard(ec.text.substring(sb, se))
            } else if (key.key == 'V') {
              replaceSelection(AppWindow.getClipboard)
            }
          }

          if (key.key == KeyEvent.Home) {
            if (key.shift) ec.selection = Span(0, se, sp)
            else           ec.selection = Span(0, 0, 0)
          }

          if (key.key == KeyEvent.End) {
            if (key.shift) ec.selection = Span(sb, tl, sp)
            else           ec.selection = Span(tl, tl, tl)
          }

          if (key.key == KeyEvent.Enter || key.key == KeyEvent.Escape) {
            stopEdit()
          }
        }

        for (key <- AppWindow.keyEvents.filter(e => e.down || e.repeat)) {

          if (key.key == KeyEvent.Left) {
            val pos = if (sb < sp) sb else se
            moveCaret(scanLeft(pos, key.control), key.shift)
          }

          if (key.key == KeyEvent.Right) {
            val pos = if (sb < sp) sb else se
            moveCaret(scanRight(pos, key.control), key.shift)
          }

          if (key.key == KeyEvent.Backspace) {
            if (se > sb) {
              replaceSelection("")
            } else {
              val pos = scanLeft(sb, key.control)
              ec.selection = Span(pos, se, sp)
              replaceSelection("")
            }
          }

          if (key.key == KeyEvent.Delete) {
            if (se > sb) {
              replaceSelection("")
            } else {
              val pos = scanLeft(se, key.control)
              ec.selection = Span(sb, pos, sp)
              replaceSelection("")
            }
          }
        }

        for (char <- AppWindow.charEvents) {
          val str = char.toString
          if (!str.exists(_.isControl)) {
            replaceSelection(str)
          }
        }

      }

    }

    if (visible) {

      if (enabled)
        inputs.add(0, input, fullBox, 0.0, 0)

      val bgColor = if (enabled) Color.White else Color.White * 0.5
      canvas.draw(0, style.idleBackgroundSprite, fullBox, bgColor)

      editContext match {
        case Some(ec) =>
          val selTs = style.selectedTextStyle.scaled(parent.unit.y)
          val Span(sb, se, sp) = ec.selection

          val x0 = pos.x
          val x1 = x0 + ts.measureWidth(ec.text, 0, sb, ec.text.lift(sb).getOrElse('\0'))
          val x2 = x1 + selTs.measureWidth(ec.text, sb, se - sb, ec.text.lift(se).getOrElse('\0'))
          val y = pos.y

          if (x2 > x1) {
            canvas.draw(0, style.selectSprite, x1, fullBox.y0 + style.selectPad, x2 - x1, fullBox.heightPx - 2.0 * style.selectPad)
          } else {
            val color = Color.White.copy(a = math.sin(AppWindow.currentTime * 8.0) * 0.2 + 0.8)
            val cw = style.caretWidth * unit.y
            canvas.draw(0, style.selectSprite, x1 - cw * 0.5, fullBox.y0 + style.selectPad, cw, fullBox.heightPx - 2.0 * style.selectPad, color)
          }

          canvas.drawText(0, ts,    x0, y, ec.text, 0, sb)
          canvas.drawText(0, selTs, x1, y, ec.text, sb, se - sb)
          canvas.drawText(0, ts,    x2, y, ec.text, se, ec.text.length - se)

        case None =>
          val text = currentText
          canvas.drawText(0, ts, pos, text)
      }
    }
  }
}

