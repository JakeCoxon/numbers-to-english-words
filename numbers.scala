object NumberToEnglishWords {

  val tens = "UNUSED UNUSED twenty thirty forty fifty sixty seventy eighty ninety".split(" ")
  val units = "UNUSED one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen".split(" ")

  def numberToString(number: Int) = number match { 
    case 0 => "zero"
    case _ => numberToList(number).mkString(" ")
  }

  def comma(list: List[String]) = if (list.size == 0) Nil else list.mkString(", ") :: Nil

  def numberToList(number: Int) = 
    (comma(thousandsToList(number / 100 * 100)), commalessNumber(number % 100)) match {
      case (high, low) if high.size > 0 && low.size > 0 => s"${high.mkString(" ")} and ${low.mkString(" ")}" :: Nil
      case (high, low) => high ++ low
    }

  def thousandsToList(number: Int): List[String] = (number match {
    case _ if number < 1000 => commalessNumber(number) :: Nil
    case _ if number < 1000000 => (numberToList(number / 1000) :+ "thousand") :: (numberToList(number % 1000) :: Nil)
    case _ if number < 1000000000 => (numberToList(number / 1000000) :+ "million") :: (numberToList(number % 1000000) :: Nil)
  }).filter(_ != Nil).map(_.mkString(" "))

  def commalessNumber(number: Int): List[String] = number match {
    case 0 => Nil
    case _ if number < 20 => units(number) :: Nil
    case _ if number < 100 => tens(number / 10) :: commalessNumber(number % 10)
    case _ if number < 1000 => units(number / 100) :: "hundred" :: Nil
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