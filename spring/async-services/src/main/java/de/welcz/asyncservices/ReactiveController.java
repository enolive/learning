package de.welcz.asyncservices;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
public class ReactiveController {
  private static final Logger LOGGER = LoggerFactory.getLogger(ReactiveController.class);
  private final ReactiveService reactiveService;

  @Autowired
  public ReactiveController(ReactiveService reactiveService) {
    this.reactiveService = reactiveService;
  }

  @GetMapping("/hello/{person}")
  public Mono<String> sayHelloTo(@PathVariable String person) {
    reactiveService.sayHelloToAsync(person).thenAccept(LOGGER::info);
    return Mono.just("I just said hello.");
  }
}
