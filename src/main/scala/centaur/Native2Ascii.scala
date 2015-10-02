package centaur

import java.nio.charset.Charset

object Native2Ascii extends App {

  def native2ascii(native: CharSequence): CharSequence = native.toString.map {
    case c if isAscii(c) => c.toString
    case c => toUnicode(c)
  }.mkString

  def ascii2native(ascii: CharSequence): CharSequence = {
    // '\\\\' is not unescaped by Scala since we use multiline string """"""
    // and will be unescaped by regex, to a normal string '\\'
    ascii.toString.split( """\\\\""").map { s =>
      val Array(head, tail@_*) = s.split( """\\u""")
      head + tail.flatMap {
        case str if str.length >= 4 =>
          val (u, normal) = str.splitAt(4)
          Seq(unescapeUnicodeChar(u), normal)
        case str => Seq(str)
      }.mkString
    }.mkString( """\\""")
  }

  private def unescapeUnicodeChar(u: String): String = Integer.parseInt(u, 16).toChar.toString

  private def isAscii(char: Char): Boolean = Charset.forName("US-ASCII").newEncoder().canEncode(char.toString)

  private def toUnicode(char: Char): String = "\\u" + Integer.toHexString(char.toInt | 0x10000).substring(1)

}
