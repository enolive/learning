package com.example

import io.kotest.assertions.json.shouldMatchJson
import io.kotest.assertions.ktor.shouldHaveStatus
import io.kotest.assertions.ktor.shouldNotHaveContentType
import io.kotest.core.spec.style.DescribeSpec
import io.ktor.http.*
import io.ktor.server.testing.*

class ApplicationTest : DescribeSpec({
  describe("application routes") {
    describe("/") {
      it("has GET") {
        withTestApplication({ module(testing = true) }) {
          handleRequest(HttpMethod.Get, "/").apply {
            response shouldHaveStatus HttpStatusCode.OK
            response.shouldNotHaveContentType(ContentType.Application.Json)
            //language=JSON
            response.content shouldMatchJson """{"text": "Hello, World!"}"""
          }
        }
      }
    }
  }
})
