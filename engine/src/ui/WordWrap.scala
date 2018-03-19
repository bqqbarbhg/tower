package ui

import collection.mutable.ArrayBuffer

object WordWrap {

  /** Is `ch` a good character to break the line at */
  def shouldWrapAt(ch: Char): Boolean = ch == '\r' || ch == ' '

  /** Is `ch` a soft hyphen */
  def isSoftHyphen(ch: Char): Boolean = ch == 0xAD

  /**
    * Wrap text into lines of maximum length `boxWidth`.
    * Manual linebreaks are also folded
    *
    * @param font Font to use for measuring the text
    * @param height Height of the font to use
    * @param boxWidth Size of the wrapping area
    * @param text Text to wrap
    * @param offset First index of `text` to use
    * @param length Number of characters to use in `text`
    * @return Lines of text to render
    */
  def wrapText(font: Font, height: Double, boxWidth: Double, text: String, offset: Int, length: Int): ArrayBuffer[String] = {
    var begin = offset
    var ix = begin
    val textEnd = offset + length

    val lines = ArrayBuffer[String]()

    var prevCh = '\0'
    var width = 0.0

    /** Flush the current line broken at `indexOfLinebreak` */
    def flushAt(indexOfLinebreak: Int): Unit = {
      var end = indexOfLinebreak
      var isShy = false

      // Eat all line-wrapping characters from the end
      if (end < textEnd) {
        isShy = isSoftHyphen(text(end))
        while (end > begin + 1 && shouldWrapAt(text(end - 1))) {
          end -= 1
        }
      }

      if (isShy) {
        lines += text.substring(offset + begin, offset + end) + '-'
      } else {
        lines += text.substring(offset + begin, offset + end)
      }

      // Eat all line-wrapping characters from the next beginning
      begin = end + 1
      while (begin < textEnd && shouldWrapAt(text(begin))) {
        begin += 1
      }

      prevCh = 0
      ix = begin
      width = 0.0
    }

    /**
      * Find best place to break the line.
      *
      * @param begin Begin of the current line to break
      * @param end End of the current line to break (index of offending character)
      * @param totalWidth Width after advanding past `end`
      */
    def findBestBreak(begin: Int, end: Int, totalWidth: Double): Int = {

      /** Get advance of character `ch` at position `ix` */
      def getAdvance(ix: Int, ch: Char): Double = if (ix > begin)
        font.getAdvance(ch, height, text(ix - 1))
      else
        font.getAdvance(ch, height)

      // Scan backwards to find best break
      var width = totalWidth
      var ix = end
      while (ix > begin + 1) {
        val ch = text(ix)
        width -= getAdvance(ix, ch)
        if (shouldWrapAt(ch)) return ix
        if (isSoftHyphen(ch)) {
          val shyWidth = width + getAdvance(ix, '-')
          if (shyWidth <= boxWidth) return ix
        }
        ix -= 1
      }

      // If none were found scan forward until line is full
      while (ix < end) {
        if (width > boxWidth) return ix

        val ch = text(ix)
        width += getAdvance(ix, ch)
        ix += 1
      }

      end
    }

    while (ix < textEnd) {
      val ch = text(ix)

      if (ch == '\n') {
        flushAt(ix)
      } else {
        width += font.getAdvance(ch, height, prevCh)
        prevCh = ch

        if (width > boxWidth) {
          // Find a good break
          var breakIx = findBestBreak(begin, ix, width)
          flushAt(breakIx)
        } else {
          ix += 1
        }
      }
    }

    flushAt(textEnd)

    lines
  }

}
