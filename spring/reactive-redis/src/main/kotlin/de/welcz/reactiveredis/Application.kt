package de.welcz.reactiveredis

import org.springframework.boot.autoconfigure.SpringBootApplication
import org.springframework.boot.runApplication
import org.springframework.cache.annotation.EnableCaching

@SpringBootApplication
@EnableCaching
class ReactiveRedisApplication

fun main(args: Array<String>) {
  runApplication<ReactiveRedisApplication>(*args)
}
