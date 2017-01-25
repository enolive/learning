import org.scalatest.{FlatSpec, Matchers}

class ConverterTest extends FlatSpec with Matchers {
    val target = new Converter

    behavior of "Converter"

    it should "convert single roman digit" in {
        target.toArabic("I") should be(1)
        target.toArabic("V") should be(5)
    }

    it should "convert multiple roman digits" in {
        target.toArabic("VII") should be(7)
        target.toArabic("XI") should be(11)
    }
}
