package de.welcz.fizzbuzz;

import io.vavr.collection.List;
import io.vavr.control.Option;
import org.springframework.stereotype.Service;

@Service
public class FizzBuzzService {

    private final List<DivByRule> rules = List.of(
            new DivByRule(3, "Fizz"),
            new DivByRule(5, "Buzz")
    );

    String calculate(int input) {
        return getResultFromRules(input).getOrElse(() -> String.valueOf(input));
    }

    private Option<String> getResultFromRules(int input) {
        return Option.of(rules.filter(rule -> rule.appliesTo(input))
                              .map(DivByRule::getResult)
                              .mkString("-"))
                     .filter(r -> !r.isEmpty());
    }

}
