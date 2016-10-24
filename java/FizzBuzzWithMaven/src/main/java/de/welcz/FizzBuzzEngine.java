package de.welcz;

import java.util.Arrays;
import java.util.List;

public class FizzBuzzEngine {

    private final List<Rule> rules = Arrays.asList(
            rule().denominator(15).givingResult("Fizz-Buzz").create(),
            rule().denominator(3).givingResult("Fizz").create(),
            rule().denominator(5).givingResult("Buzz").create());

    public String calculateNext(final int number) {
        return rules.stream()
                .filter(r -> r.appliesTo(number))
                .map(Rule::getResult)
                .findFirst()
                .orElse(Integer.toString(number));
    }

    private RuleBuilder rule() {
        return new RuleBuilder();
    }
}
