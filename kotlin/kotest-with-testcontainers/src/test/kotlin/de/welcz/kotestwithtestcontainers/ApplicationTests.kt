package de.welcz.kotestwithtestcontainers

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.nulls.shouldNotBeNull
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.core.env.Environment
import org.springframework.test.context.ActiveProfiles

@SpringBootTest
@ActiveProfiles("testcontainers")
class ApplicationTests(private val environment: Environment) : DescribeSpec({
  describe("Application") {
    it("has a working environment") {
      environment.shouldNotBeNull()
    }
  }
})
