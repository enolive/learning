package de.welcz.kotestwithtestcontainers

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.equality.shouldBeEqualToIgnoringFields
import io.kotest.matchers.nulls.shouldNotBeNull
import io.kotest.matchers.shouldBe
import org.springframework.boot.test.context.SpringBootTest


@SpringBootTest
class BeerRepositoryTest(private val sut: BeerRepository) : DescribeSpec({
  describe("Beer repository") {
    it("stores beer") {
      val beer = Beer(
        id = null,
        brand = "Schanze",
        name = "Rot",
        strength = 5.0.toBigDecimal(),
      )

      val saved = sut.save(beer)

      saved.shouldBeEqualToIgnoringFields(beer, Beer::id)
      saved.id.shouldNotBeNull()
      sut.findById(saved.id!!) shouldBe saved
    }
  }
}) {
  companion object {
    init {
      TestDatabase.start()
    }
  }
}

