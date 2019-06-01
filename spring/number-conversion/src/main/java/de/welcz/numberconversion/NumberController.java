package de.welcz.numberconversion;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.math.BigDecimal;

@RestController
public class NumberController {
  private final NumberConverter numberConverter;

  @Autowired
  public NumberController(NumberConverter numberConverter) {
    this.numberConverter = numberConverter;
  }

  @GetMapping(value = "/number/{number}")
  public Mono<BigDecimal> getNumber(@PathVariable String number) {
    return Mono.just(numberConverter.toBigDecimal(number));
  }
}
