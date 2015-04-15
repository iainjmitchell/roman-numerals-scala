import org.scalatest._

class RomanSpec extends FunSuite with Matchers {
  val ArabicToNumeralMappings = List(
    new ArabicNumeralMapping(1, "I"),
    new ArabicNumeralMapping(4, "IV"),
    new ArabicNumeralMapping(5, "V"),
    new ArabicNumeralMapping(9, "IX"),
    new ArabicNumeralMapping(10, "X"),
    new ArabicNumeralMapping(40, "XL"),
    new ArabicNumeralMapping(50, "L"),
    new ArabicNumeralMapping(90, "XC"),
    new ArabicNumeralMapping(100, "C"),
    new ArabicNumeralMapping(500, "D"),
    new ArabicNumeralMapping(900, "CM"),
    new ArabicNumeralMapping(1000, "M")
  )
  
  ArabicToNumeralMappings.foreach{
    case (arabicNumber) =>
      test(arabicNumber + " returns " + arabicNumber.numeral){
        new NumeralConvertor(ArabicToNumeralMappings)
          .convert(arabicNumber.arabic) should equal(arabicNumber.numeral)
      }
  }

  test("2 returns II"){
    new NumeralConvertor(ArabicToNumeralMappings)
      .convert(2) should equal("II")
  }

  test("6 returns VI"){
    new NumeralConvertor(ArabicToNumeralMappings)
      .convert(6) should equal("VI")
  }

  test("78 returns LXXVIII"){
    new NumeralConvertor(ArabicToNumeralMappings)
      .convert(78) should equal("LXXVIII")
  }
}

case class ArabicNumeralMapping(arabic: Int, numeral: String)

class NumeralConvertor(arabicToNumeralMap: List[ArabicNumeralMapping]) {
  private val sortedArabicToNumeralMap = List(arabicToNumeralMap.toSeq.sortWith(_.arabic > _.arabic):_*)

  def convert(arabicNumber: Int): String  = {
    toRoman(arabicNumber, sortedArabicToNumeralMap)
  }

  private def toRoman(arabicNumber: Int, arabicToNumeralMap: List[ArabicNumeralMapping]) : String = arabicToNumeralMap match {
    case Nil => ""
    case head :: tail => head.numeral * (arabicNumber / head.arabic) + toRoman(arabicNumber % head.arabic, tail)
  }
}
