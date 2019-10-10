package de.welcz.kafkapublishing;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@RestController
public class Controller {
  private final Producer producer;

  @Autowired
  public Controller(Producer producer) {
    this.producer = producer;
  }

  @PostMapping("/string")
  public Mono<Void> publishString(@RequestBody String payload) {
    return producer.publishSimpleMessage(payload)
                   .then();
  }

  @PostMapping("/shit")
  public Mono<Void> publishShit(@RequestBody @Valid Shit shit) {
    return producer.publishShit(shit)
                   .then();
  }
}
