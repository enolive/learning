package de.welcz.excelexport

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.asFlow
import kotlinx.coroutines.launch
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.springframework.core.io.buffer.DataBuffer
import org.springframework.core.io.buffer.DataBufferFactory
import org.springframework.http.ContentDisposition
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity
import org.springframework.http.server.reactive.ServerHttpResponse
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RestController
import java.io.InputStream
import java.io.PipedInputStream
import java.io.PipedOutputStream

@RestController
class ExcelExportController {
  @GetMapping("export")
  fun export(response: ServerHttpResponse): ResponseEntity<Flow<DataBuffer>> {
    val workBook = createExcelDocument()
    val scope = CoroutineScope(Dispatchers.IO)
    val output = PipedOutputStream()
    val input = PipedInputStream(output)

    // WARN: closing the work book or joining the thread will result in strange behaviour
    // the work book is meant to be streamed and thus cannot be prematurely closed!

    // WARN: you will get a deadlock if trying to write to the output stream in the main thread!

    @Suppress("BlockingMethodInNonBlockingContext")
    scope.launch {
      workBook.use {
        output.use {
          workBook.write(output)
        }
      }
    }

    // 256 KiB might be a better chunk size!
    return input.toDataBufferFlow(response.bufferFactory(), 1024).asFileAttachment()
  }

  // NOTE: using Kotlin flow generator instead of the awkward DataBufferUtils
  private fun InputStream.toDataBufferFlow(
    dataBufferFactory: DataBufferFactory,
    maxBufferSize: Int
  ) = buffered()
    .iterator()
    .asSequence()
    .chunked(maxBufferSize)
    .map { dataBufferFactory.wrap(it.toByteArray()) }
    .asFlow()
}

private fun Flow<DataBuffer>.asFileAttachment(): ResponseEntity<Flow<DataBuffer>> = ResponseEntity
  .ok()
  .headers {
    it.contentType = MediaType.parseMediaType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    it.contentDisposition = ContentDisposition.attachment().filename("Export.xlsx").build()
  }
  .body(this)

fun createExcelDocument(): XSSFWorkbook {
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
