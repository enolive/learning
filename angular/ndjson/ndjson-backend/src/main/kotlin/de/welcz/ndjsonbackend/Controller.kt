package de.welcz.ndjsonbackend

import kotlinx.coroutines.flow.flowOf
import kotlinx.coroutines.flow.map
import org.springframework.http.MediaType
import org.springframework.web.bind.annotation.GetMapping
import org.springframework.web.bind.annotation.RequestMapping
import org.springframework.web.bind.annotation.RestController

@RestController
@RequestMapping("/api/v1/hellos")
class Controller {
  @GetMapping(produces = [MediaType.APPLICATION_NDJSON_VALUE])
  fun hellos() =
    flowOf(
      "Hello World!",
      "Hello everyone!",
      "Hello folks!",
    ).map(::Greetings)
}

