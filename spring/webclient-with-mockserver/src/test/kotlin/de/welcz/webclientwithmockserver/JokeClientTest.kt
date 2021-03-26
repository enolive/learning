package de.welcz.webclientwithmockserver

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import okhttp3.mockwebserver.MockResponse
import okhttp3.mockwebserver.MockWebServer
import org.intellij.lang.annotations.Language
import org.springframework.boot.test.autoconfigure.web.client.AutoConfigureWebClient
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.context.ContextConfiguration

@SpringBootTest
@ContextConfiguration(classes = [JokeClient::class])
@AutoConfigureWebClient
@Suppress("BlockingMethodInNonBlockingContext")
class JokeClientTest(
  private val sut: JokeClient,
) : DescribeSpec({
  val originalBaseUrl = sut.baseUrl
  lateinit var mockWebServer: MockWebServer

  beforeSpec {
    mockWebServer = MockWebServer()
    mockWebServer.start()
    sut.baseUrl = "http://localhost:${mockWebServer.port}"
  }

  afterSpec {
    mockWebServer.shutdown()
  }

  describe("client for jokes") {
    it("uses the correct url to the API") {
      originalBaseUrl shouldBe "https://api.chucknorris.io"
    }

    it("calls chuck norris api") {
      @Language("JSON") val json = """{"value": "Test Joke"}"""
      mockWebServer.enqueue(MockResponse().setBody(json).addHeader("Content-Type", "application/json"))

      val result = sut.fetchRandomJoke()

      result shouldBe JokeResponse("Test Joke")
      val request = mockWebServer.takeRequest()
      request.path shouldBe "/jokes/random"
    }
  }
})
