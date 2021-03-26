package de.welcz.ktorclient

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import io.ktor.client.*
import io.ktor.client.engine.java.*
import io.ktor.client.features.json.*
import mu.KLogging
import org.springframework.context.annotation.Bean
import org.springframework.context.annotation.Configuration

@Configuration
class ClientConfig(private val objectMapper: ObjectMapper) {
  companion object : KLogging()

  @Bean
  fun httpClient() = HttpClient(Java) {
    install(JsonFeature) {
      serializer = JacksonSerializer(objectMapper) {
        disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES)
      }
    }
  }
}
