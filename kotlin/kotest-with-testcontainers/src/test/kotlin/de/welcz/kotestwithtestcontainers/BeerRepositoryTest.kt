package de.welcz.kotestwithtestcontainers

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.optional.shouldBePresent
import io.kotest.matchers.shouldBe
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ActiveProfiles

@SpringBootTest
@ActiveProfiles("testcontainers")
class BeerRepositoryTest(private val sut: BeerRepository) : DescribeSpec({
  describe("Beer repository") {
    it("stores beer") {
      val beer = Beer(
        brand = "Schanze",
        name = "Rot",
        strength = 5.0.toBigDecimal(),
      )

      val saved = sut.save(beer)

      saved shouldBe beer
      sut.findById(saved.id!!).shouldBePresent() shouldBe saved
    }
  }
})

