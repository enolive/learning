package de.welcz.ktorclient

import com.fasterxml.jackson.databind.ObjectMapper
import io.kotest.assertions.json.shouldMatchJson
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.core.spec.style.featureSpec
import io.ktor.client.*
import io.ktor.client.engine.mock.*
import io.ktor.client.features.json.*
import io.ktor.http.*
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest
import org.springframework.boot.test.context.TestConfiguration
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Import
import org.springframework.test.web.reactive.server.WebTestClient
import org.springframework.test.web.reactive.server.expectBody

@WebFluxTest(controllers = [JokeController::class])
@Import(TestClientConfig::class)
class JokeControllerTest(
  private val webTestClient: WebTestClient,
  private val httpClient: HttpClient,
) : DescribeSpec({
  describe("API for /jokes") {
    it("has GET /random") {
      val response = webTestClient.get().uri("/jokes/random").exchange()

      response.expectStatus().isOk
      response.expectBody<String>().consumeWith {
        it.responseBody shouldMatchJson """{"value": "Test Joke"}"""
      }
    }
  }
})

@TestConfiguration
class TestClientConfig(private val objectMapper: ObjectMapper) {
  @Bean
  fun httpClient() = HttpClient(MockEngine) {
    install(JsonFeature) {
      serializer = JacksonSerializer(objectMapper)
    }
    engine {
      addHandler { request ->
        when (request.url.fullPath) {
          "/jokes/random" -> respond(
            content = """{"value": "Test Joke"}""",
            headers = headersOf("Content-Type" to listOf("application/json"))
          )
          else            -> error("url unknown ${request.url.fullPath}")
        }
      }
    }
  }
}

