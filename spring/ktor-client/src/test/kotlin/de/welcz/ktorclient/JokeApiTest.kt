package de.welcz.ktorclient

import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import io.ktor.client.*
import io.ktor.client.engine.mock.*
import io.ktor.client.features.json.*
import io.ktor.http.*

class JokeApiTest : DescribeSpec({
  val httpClient = HttpClient(MockEngine) {
    install(JsonFeature) {
      serializer = JacksonSerializer()
    }
    engine {
      addHandler { request ->
        when (request.url.toString()) {
          "https://api.chucknorris.io/jokes/random" -> respond(
            content = """{"value": "Test Joke"}""",
            headers = headersOf("Content-Type" to listOf("application/json"))
          )
          else                                      -> error("wrong url ${request.url}")
        }
      }
    }
  }

  describe("joke API") {
    it("fetches random joke") {
      val sut = JokeApi(httpClient)

      val result = sut.fetchRandomJoke()

      result shouldBe JokeResponse("Test Joke")
    }
  }
})
