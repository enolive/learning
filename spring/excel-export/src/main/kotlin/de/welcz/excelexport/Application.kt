package de.welcz.excelexport

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication

@SpringBootApplication
class ExcelExportApplication

fun main(args: Array<String>) {
  runApplication<ExcelExportApplication>(*args)
}
