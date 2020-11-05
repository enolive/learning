package de.welcz.gatewayforking

import org.springframework.cloud.gateway.route.RouteLocator
import org.springframework.cloud.gateway.route.builder.GatewayFilterSpec
import org.springframework.cloud.gateway.route.builder.RouteLocatorBuilder
import org.springframework.context.annotation.Bean
import org.springframework.http.HttpMethod
import org.springframework.stereotype.Component
import org.springframework.web.reactive.function.client.WebClient
import org.springframework.web.reactive.function.client.bodyToMono
import org.springframework.web.server.ServerWebExchange
import reactor.core.publisher.Flux
import reactor.core.publisher.Mono

@Component
class Routes(private val builder: WebClient.Builder) {
  @Bean
  fun customRouteLocator(builder: RouteLocatorBuilder): RouteLocator =
      builder
        .routes()
        .route("hellos") { r ->
          r.path("/hellos/**")
            .filters { f ->
              f.modifyRequestBody(fork("http://localhost:8082", "http://localhost:8083"))
            }
            .uri("http://localhost:8081")
        }
        .build()

  private fun fork(vararg uris: String): (exchange: ServerWebExchange, body: String?) -> Mono<String> =
      { exchange, body ->
        Flux.just(*uris)
          .flatMap { callWebClient(exchange, body, it) }
          .collectList()
          .thenReturn(body ?: "")
      }

  private fun callWebClient(exchange: ServerWebExchange, body: String?, uri: String): Mono<String> =
      builder.baseUrl(uri)
        .build()
        .method(exchange.request.method ?: HttpMethod.GET)
        .uri {
          it
            .path(exchange.request.path.toString())
            .queryParams(exchange.request.queryParams)
            .build()
        }
        .headers { it.putAll(exchange.request.headers) }
        .bodyValue(body ?: "")
        .retrieve()
        .bodyToMono<String>()
        .log()
}

inline fun <reified I, reified O> GatewayFilterSpec.modifyRequestBody(
    noinline function: (ServerWebExchange, I?) -> Mono<O>): GatewayFilterSpec =
    modifyRequestBody(I::class.java, O::class.java, function)