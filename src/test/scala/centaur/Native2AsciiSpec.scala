package centaur

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

class Native2AsciiSpec extends FunSuite with Matchers with PropertyChecks {
  import Native2Ascii._

  case class AsciiChar(char: Char)
  case class NonAsciiChar(char: Char)

  implicit val asciiCharArbitrary: Arbitrary[AsciiChar] = Arbitrary(
    Gen.choose(32, 127).map(_.toChar).map(AsciiChar)
  )

  implicit val nonAsciiCharArbitrary: Arbitrary[NonAsciiChar] = Arbitrary(
    Arbitrary.arbitrary[Char].filter(c => c.toInt < 32 || c.toInt >= 128).map(NonAsciiChar)
  )

  test("native2ascii should convert ascii & non-ascii chars correctly") {
    assert(native2ascii("").toString == "")
    assert(native2ascii("路·").toString == "\\u8def\\u00b7")
    assert(native2ascii("中abc").toString == "\\u4e2dabc")
    assert(native2ascii("声明：此为贴吧助手插件的功能，不是百度贴吧原有功能～。设置称呼后，在帖子页面，您设置的称呼将会自动显示在自己的头像上。").toString ==
      "\\u58f0\\u660e\\uff1a\\u6b64\\u4e3a\\u8d34\\u5427\\u52a9\\u624b\\u63d2\\u4ef6\\u7684\\u529f\\u80fd\\uff0c\\u4e0d\\u662f\\u767e\\u5ea6\\u8d34\\u5427\\u539f\\u6709\\u529f\\u80fd\\uff5e\\u3002\\u8bbe\\u7f6e\\u79f0\\u547c\\u540e\\uff0c\\u5728\\u5e16\\u5b50\\u9875\\u9762\\uff0c\\u60a8\\u8bbe\\u7f6e\\u7684\\u79f0\\u547c\\u5c06\\u4f1a\\u81ea\\u52a8\\u663e\\u793a\\u5728\\u81ea\\u5df1\\u7684\\u5934\\u50cf\\u4e0a\\u3002")
  }

  test("ascii2native should convert escaped strings to original string correctly") {
    assert(ascii2native("\\u4e2dabc").toString == "中abc")
    assert(ascii2native("\\u8def\\u00b7").toString == "路·")
    assert(ascii2native("\\u58f0\\u660e\\uff1a\\u6b64\\u4e3a\\u8d34\\u5427\\u52a9\\u624b\\u63d2\\u4ef6\\u7684\\u529f\\u80fd\\uff0c\\u4e0d\\u662f\\u767e\\u5ea6\\u8d34\\u5427\\u539f\\u6709\\u529f\\u80fd\\uff5e\\u3002\\u8bbe\\u7f6e\\u79f0\\u547c\\u540e\\uff0c\\u5728\\u5e16\\u5b50\\u9875\\u9762\\uff0c\\u60a8\\u8bbe\\u7f6e\\u7684\\u79f0\\u547c\\u5c06\\u4f1a\\u81ea\\u52a8\\u663e\\u793a\\u5728\\u81ea\\u5df1\\u7684\\u5934\\u50cf\\u4e0a\\u3002").toString == "声明：此为贴吧助手插件的功能，不是百度贴吧原有功能～。设置称呼后，在帖子页面，您设置的称呼将会自动显示在自己的头像上。")
  }

  test("ascii string should remain unchanged when native2ascii") {
    forAll { chars: List[AsciiChar] =>
      val str = new String(chars.map(_.char).toArray)
      native2ascii(str) shouldEqual str
    }
  }

  test("ascii string without `\\u` should remain unchanged when ascii2native") {
    forAll { chars: List[AsciiChar] =>
      val str = new String(chars.map(_.char).toArray)
      whenever(!str.contains("\\u")) {
        ascii2native(str) shouldBe str
      }
    }
  }

  test("Non-Ascii string should be converted") {
    forAll { char: NonAsciiChar =>
      val str = char.char.toString
      native2ascii(str) shouldNot be(str)
    }
  }

  test("string should be converted forward and backward") {
    forAll { str: String =>
      ascii2native(native2ascii(str)) shouldBe str
    }
  }

  test("special case with escaped \\") {
    native2ascii("\\\\u1234") shouldBe "\\\\u1234"
    ascii2native("\\\\u1234") shouldBe "\\\\u1234"

    native2ascii("\\\\\\u1234") shouldBe "\\\\\\u1234"
    ascii2native("\\\\\\u1234") shouldBe "\\\\ሴ"
  }

}
