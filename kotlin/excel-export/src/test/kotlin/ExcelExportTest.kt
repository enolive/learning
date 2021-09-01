import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.collections.shouldNotBeEmpty
import io.kotest.matchers.shouldBe
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import org.apache.poi.xssf.extractor.XSSFExcelExtractor
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import java.io.PipedInputStream
import java.io.PipedOutputStream

class ExcelExportTest : DescribeSpec({
  describe("export to excel") {
    it("exports expected data") {
      val workbook = createExcelDocument()

      val extractor = XSSFExcelExtractor(workbook)
      extractor.text shouldBe """Sheet0
Cell 1 1	Cell 1 2
Cell 2 1	Cell 2 2
"""
    }

    it("writes data to output") {
      val scope = CoroutineScope(Dispatchers.IO)
      val workbook = createExcelDocument()
      val output = PipedOutputStream()
      val input = PipedInputStream(output)

      val job = scope.launch {
        workbook.write(output)
        output.close()
      }

      input.readAllBytes().toList().shouldNotBeEmpty()
      job.join()
    }
  }
})

private fun createExcelDocument(): XSSFWorkbook {
  val workbook = XSSFWorkbook()
  val sheet = workbook.createSheet()
  val row1 = sheet.createRow(0)
  val row2 = sheet.createRow(1)
  val cell11 = row1.createCell(0)
  cell11.setCellValue("Cell 1 1")
  val cell12 = row1.createCell(1)
  cell12.setCellValue("Cell 1 2")
  val cell21 = row2.createCell(0)
  cell21.setCellValue("Cell 2 1")
  val cell22 = row2.createCell(1)
  cell22.setCellValue("Cell 2 2")
  return workbook
}
