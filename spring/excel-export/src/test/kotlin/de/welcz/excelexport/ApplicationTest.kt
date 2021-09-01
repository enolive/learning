package de.welcz.excelexport

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.nulls.shouldNotBeNull
import org.junit.jupiter.api.Test
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.core.env.Environment

@SpringBootTest
class ApplicationTest(private val environment: Environment) : DescribeSpec({
  describe("Application") {
    it("has a working environment") {
      environment.shouldNotBeNull()
    }
  }
})
