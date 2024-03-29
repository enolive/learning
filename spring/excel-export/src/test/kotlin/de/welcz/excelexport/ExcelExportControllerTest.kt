package de.welcz.excelexport

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.collections.shouldHaveSize
import io.kotest.matchers.nulls.shouldNotBeNull
import io.kotest.matchers.shouldBe
import org.apache.poi.xssf.extractor.XSSFExcelExtractor
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.http.ContentDisposition
import org.springframework.test.web.reactive.server.WebTestClient

@WebFluxTest
class ExcelExportControllerTest(private val webTestClient: WebTestClient) : DescribeSpec({
  describe("Excel export") {
    it("streams the generated excel file") {
      val expectedSize = 3324
      val expectedContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
      val expectedContentDisposition = ContentDisposition.parse("""attachment; filename="Export.xlsx"""")

      val response = webTestClient.get().uri("/export").exchange()

      response.expectStatus().isOk
      response.expectHeader().contentType(expectedContentType)
      response.expectHeader().contentDisposition(expectedContentDisposition)
      response.expectBody().consumeWith {
        it.responseBody.shouldNotBeNull().toList().shouldHaveSize(expectedSize)
      }
    }

    it("generates the expected file") {
      val workbook = createExcelDocument()
      val expected = """
      Sheet0
      Cell 1 1	Cell 1 2
      Cell 2 1	Cell 2 2
      """.trimIndent()
      val extractor = XSSFExcelExtractor(workbook)

      extractor.text.trim() shouldBe expected
    }
  }
})
