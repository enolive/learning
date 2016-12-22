import org.scalatest.{FlatSpec, Matchers}

class ConverterTest extends FlatSpec with Matchers {
    val converter = new Converter

    behavior of "Converter"

    it should "convert single roman digit" in {
        converter.toArabic("I") should be(1)
        converter.toArabic("V") should be(5)
    }

    it should "convert multiple roman digits" in {
        converter.toArabic("VII") should be(7)
        converter.toArabic("XI") should be(11)
    }
}
