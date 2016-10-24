package de.welcz;

import com.google.common.collect.ImmutableList;

import java.util.List;

public class FizzBuzzEngine {

    private final List<Rule> rules = ImmutableList.of(
            aRule().forDenominator(15).givingResult("Fizz-Buzz").create(),
            aRule().forDenominator(3).givingResult("Fizz").create(),
            aRule().forDenominator(5).givingResult("Buzz").create());

    public String calculateNext(final int number) {
        return rules.stream()
                .filter(r -> r.appliesTo(number))
                .map(Rule::getResult)
                .findFirst()
                .orElse(Integer.toString(number));
    }

    private RuleBuilder aRule() {
        return new RuleBuilder();
    }
}
