package de.welcz;

import com.google.common.collect.ImmutableList;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class FizzBuzzEngine {

    private final List<Rule> rules = aListOfRules(
            aRule().forDenominator(15).givingResult("Fizz-Buzz"),
            aRule().forDenominator(3).givingResult("Fizz"),
            aRule().forDenominator(5).givingResult("Buzz"));

    @NotNull
    private static RuleBuilder aRule() {
        return new RuleBuilder();
    }

    @NotNull
    private List<Rule> aListOfRules(RuleBuilder... rules) {
        Stream<Rule> stream = Arrays.stream(rules).map(RuleBuilder::create);
        return ImmutableList.copyOf(stream.iterator());
    }

    @NotNull
    public String calculateNext(final int number) {
        return rules.stream()
                .filter(r -> r.appliesTo(number))
                .map(Rule::getResult)
                .findFirst()
                .orElse(Integer.toString(number));
    }
}
