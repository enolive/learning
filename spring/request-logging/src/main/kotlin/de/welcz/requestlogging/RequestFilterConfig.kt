package de.welcz.requestlogging

import mu.KLogging
import org.springframework.context.annotation.Bean
import org.springframework.core.io.buffer.DataBuffer
import org.springframework.http.server.reactive.ServerHttpRequestDecorator
import org.springframework.stereotype.Component
import org.springframework.web.server.ServerWebExchange
import org.springframework.web.server.ServerWebExchangeDecorator
import org.springframework.web.server.WebFilter
import reactor.core.publisher.Flux
import java.nio.charset.StandardCharsets

@Component
class RequestFilterConfig {
  companion object : KLogging()

  @Bean
  fun logRequestBody() = WebFilter { exchange, chain ->
    when (logger.isDebugEnabled) {
      true -> chain.filter(decorateWithRequestBodyLogging(exchange))
      else -> chain.filter(exchange)
    }
  }

  private fun decorateWithRequestBodyLogging(exchange: ServerWebExchange) =
    object : ServerWebExchangeDecorator(exchange) {
      override fun getRequest() =
        object : ServerHttpRequestDecorator(exchange.request) {
          override fun getBody(): Flux<DataBuffer> {
            logger.debug("payload from request ${exchange.request.method} ${exchange.request.uri}")
            val wholeBody = mutableListOf<String>()
            return super.getBody()
              .doOnNext { dataBuffer ->
                wholeBody += StandardCharsets.UTF_8.decode(dataBuffer.asByteBuffer()).toString()
                // XXX: simple prevention of DOS by blowing up your memory
                if (wholeBody.size > 100) {
                  logger.debug(wholeBody.joinToString(""))
                  wholeBody.clear()
                }
              }
              .doOnComplete { logger.debug(wholeBody.joinToString("")) }
          }
        }
    }
}
