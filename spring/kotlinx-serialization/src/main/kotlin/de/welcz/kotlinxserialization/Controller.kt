package de.welcz.kotlinxserialization

import io.github.oshai.kotlinlogging.KotlinLogging
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import org.springframework.web.bind.annotation.*

@RestController
@RequestMapping("controller/users")
class Controller {
  private val logger = KotlinLogging.logger {}

  @GetMapping("")
  fun getUser() = User("Chris", "Welcz")

  @PostMapping("")
  fun createUser(@RequestBody user: User): User {
    val encoded = Json.encodeToString(user)
    logger.info { "encoded = $encoded" }
    return user
  }
}