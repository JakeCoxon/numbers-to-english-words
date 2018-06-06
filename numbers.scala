// sbt run

object NumberToEnglishWords {

  //
  // The algorithm works in two parts
  // in the case of 56,945,781
  // high part = 56,945,700 which becomes 
  //   [fifty six million, nine hundred and forty five thousand, seven hundred]
  //   (joining each thousands with a comma)
  // and low part = 81 which becomes [eighty one]
  // then both high and low parts are joined with an 'and'
  //

  val tens = "UNUSED UNUSED twenty thirty forty fifty sixty seventy eighty ninety".split(" ")
  val units = "UNUSED one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen".split(" ")

  def numberToString(number: Int) = 
    if (number == 0) "zero" else nonZeroNumberToString(number)

  def nonZeroNumberToString(number: Int): String = 
    highNumberToString(number / 100 * 100) joinAnd lowNumberToString(number % 100)

  // All numbers modulo 100
  def highNumberToString(number: Int): String = number match {
    case 0 => ""
    case _ if number < 1000 => units(number / 100) + " hundred"
    case _ if number < 1000000 => (nonZeroNumberToString(number / 1000) + " thousand") joinComma nonZeroNumberToString(number % 1000)
    case _ if number < 1000000000 => (nonZeroNumberToString(number / 1000000) + " million") joinComma nonZeroNumberToString(number % 1000000)
  }

  // Numbers less than 100
  def lowNumberToString(number: Int): String = number match {
    case 0 => ""
    case _ if number < 20 => units(number)
    case _ if number < 100 => tens(number / 10) joinSpace lowNumberToString(number % 10)
  }

  implicit class StringOps(val a1: String) extends AnyVal {
    // Joins left and right strings with a seperator if neither are empty
    def join (sep: String)(a2: String): String = (a1, a2) match {
      case (x, "") => x
      case ("", x) => x
      case (x, y) => s"$x$sep$y"
    }
    def joinSpace = join(" ")_
    def joinComma = join(", ")_
    def joinAnd = join(" and ")_
  }

  

  def main(args: Array[String]): Unit = {

    def test(number: Int, expected: String) = {
      val actual = numberToString(number)
      if (actual == expected) println(s"[√] $number == $actual")
      else println(s"[ ] $number == $actual (expected $expected)")
    }

    test(0, "zero")
    test(1, "one")
    test(2, "two")
    test(20, "twenty")
    test(21, "twenty one")
    test(22, "twenty two")
    test(29, "twenty nine")
    test(129, "one hundred and twenty nine")
    test(264, "two hundred and sixty four")
    test(900, "nine hundred")
    test(990, "nine hundred and ninety")
    test(945, "nine hundred and forty five")
    test(1990, "one thousand, nine hundred and ninety")
    test(10900, "ten thousand, nine hundred")
    test(100900, "one hundred thousand, nine hundred")
    test(10000, "ten thousand")
    test(10001, "ten thousand and one")
    test(100000, "one hundred thousand")
    test(5781, "five thousand, seven hundred and eighty one")
    test(945781, "nine hundred and forty five thousand, seven hundred and eighty one")
    test(945001, "nine hundred and forty five thousand and one")
    test(945201, "nine hundred and forty five thousand, two hundred and one")
    test(56945781, "fifty six million, nine hundred and forty five thousand, seven hundred and eighty one")
    test(987654321, "nine hundred and eighty seven million, six hundred and fifty four thousand, three hundred and twenty one")
    test(900000000, "nine hundred million")
    test(920000000, "nine hundred and twenty million")
    test(928000000, "nine hundred and twenty eight million")
    test(900000801, "nine hundred million, eight hundred and one")
    test(900800001, "nine hundred million, eight hundred thousand and one")

  }

}