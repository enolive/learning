package de.welcz.reactivevalidation;

import org.springframework.stereotype.Service;
import org.springframework.validation.annotation.Validated;
import reactor.core.publisher.Mono;

import javax.validation.Valid;

@Service
@Validated
public class OldSchoolValidator {
  Mono<Void> validateIt(@Valid Master master) {
    return Mono.empty();
  }
}
