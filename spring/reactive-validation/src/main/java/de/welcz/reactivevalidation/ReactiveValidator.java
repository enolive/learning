package de.welcz.reactivevalidation;

import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import javax.validation.ConstraintViolation;
import javax.validation.ConstraintViolationException;
import javax.validation.Validator;
import java.util.Set;

@Service
public class ReactiveValidator {
  private final Validator validator;

  public ReactiveValidator(Validator validator) {
    this.validator = validator;
  }

  Mono<Void> validateIt(Master master) {
    return Mono.just(validator.validate(master))
               .flatMap(this::produceErrorOnConstraintViolationsPresent);
  }

  private Mono<Void> produceErrorOnConstraintViolationsPresent(Set<ConstraintViolation<Master>> constraintViolations) {
    return constraintViolations.isEmpty()
        ? Mono.empty()
        : Mono.error(new ConstraintViolationException(constraintViolations));
  }
}
