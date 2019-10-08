package de.welcz.reactivevalidation;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import javax.validation.ConstraintViolationException;
import javax.validation.Valid;
import java.text.MessageFormat;

@RestController
public class Controller {
  private final OldSchoolValidator oldSchoolValidator;
  private final ReactiveValidator reactiveValidator;

  @Autowired
  public Controller(OldSchoolValidator oldSchoolValidator, ReactiveValidator reactiveValidator) {
    this.oldSchoolValidator = oldSchoolValidator;
    this.reactiveValidator = reactiveValidator;
  }

  @PostMapping("/data/validated-in-controller")
  public Mono<Void> insertSomethingWithValidationInController(@RequestBody @Valid Master master) {
    return Mono.empty();
  }

  @PostMapping("/data/validated-in-service")
  public Mono<Void> insertSomethingValidatedInService(@RequestBody Master master) {
    return oldSchoolValidator.validateIt(master);
  }

  @PostMapping("/data/validated-in-reactive-service")
  public Mono<Void> insertSomethingValidatedInReactiveService(@RequestBody Master master) {
    return reactiveValidator.validateIt(master)
                            .onErrorMap(ConstraintViolationException.class, this::failWithBadRequest);
  }

  private ResponseStatusException failWithBadRequest(ConstraintViolationException e) {
    return new ResponseStatusException(HttpStatus.BAD_REQUEST, MessageFormat.format(
        "the parameters were invalid:\n\n{0}",
        e.getLocalizedMessage()));
  }
}
