package centaur

import java.nio.charset.Charset

object Native2Ascii {

  def native2ascii(native: CharSequence): CharSequence = native.toString.map {
    case c if isAscii(c) => c.toString
    case c => toUnicode(c)
  }.mkString

  def ascii2native(ascii: CharSequence): CharSequence = {
    val Array(head, tail@_*) = ascii.toString.split( """\\u""")
    head + tail.flatMap {
      case str if str.length >= 4 =>
        val (u, normal) = str.splitAt(4)
        Seq(Integer.parseInt(u, 16).toChar.toString, normal)
      case str => Seq(str)
    }.mkString
  }

  private def isAscii(char: Char): Boolean = Charset.forName("US-ASCII").newEncoder().canEncode(char.toString)

  private def toUnicode(char: Char): String = "\\u" + Integer.toHexString(char.toInt | 0x10000).substring(1)

}
