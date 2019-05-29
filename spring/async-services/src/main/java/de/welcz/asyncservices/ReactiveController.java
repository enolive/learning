package de.welcz.asyncservices;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

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
    // fire and forget with @Async
    // - needs @EnableAsync
    // - poor API
    // - difficult scheduler configuration via special AsyncConfig
    // + async call is transparent
    // + easy to use
    // + no special concerns needed for testing
    reactiveService.sayHelloToAsync(person).thenAccept(LOGGER::info);
    return Mono.just("I just said hello.");
  }

  @GetMapping("/helloR/{person}")
  public Mono<String> sayHelloToR(@PathVariable String person) {
    // fire and forget with reactor
    // - a little difficult to understand publish and subscribe
    // - works with any callables
    // - hard to test
    // + easy to choose different schedulers by publishOn(Schedulers...)
    // + rich API
    // + no @EnableAsync needed
    reactiveService.sayHelloToReactive(person)
                   .publishOn(Schedulers.elastic())
                   .doOnSuccess(LOGGER::info)
                   .subscribe();
    return Mono.just("I just said hello reactive.");
  }
}
