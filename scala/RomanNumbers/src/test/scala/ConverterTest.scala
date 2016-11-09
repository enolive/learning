import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class ConverterTest extends FlatSpec with Matchers with BeforeAndAfterEach {
  var target: Converter = _

  override protected def beforeEach(): Unit = {
    target = new Converter()
  }

  behavior of "Roman digit converter"

  it should "convert I to 1" in {
    target.convert("I") should be(1)
  }

  it should "convert V to 5" in {
    target.convert("V") should be(5)
  }

  it should "convert X to 10" in {
    target.convert("X") should be(10)
  }

  it should "convert L to 50" in {
    target.convert("L") should be(50)
  }

  it should "convert C to 100" in {
    target.convert("C") should be(100)
  }

  it should "convert D to 500" in {
    target.convert("D") should be(500)
  }

  it should "convert M to 1000" in {
    target.convert("M") should be(1000)
  }
}