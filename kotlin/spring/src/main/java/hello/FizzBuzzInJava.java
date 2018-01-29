package hello;

import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

//@Service
public class FizzBuzzInJava implements FizzBuzzService {
    private List<Rule> rules = Arrays.asList(
            new Rule(isDivisibleBy(3), "Fizz"),
            new Rule(isDivisibleBy(5), "Buzz")
    );

    private Predicate<Integer> isDivisibleBy(int divisor) {
        return number -> number % divisor == 0;
    }

    @Override
    public String[] calculateUpTo(int limit) {
        return Stream
                .iterate(1, n -> n + 1)
                .map(this::calculate)
                .limit(limit)
                .toArray(String[]::new);
    }

    private String calculate(int number) {
        return orIfEmpty(
                rules.stream()
                        .filter(rule -> rule.appliesTo(number))
                        .map(Rule::getResult)
                        .collect(Collectors.joining("-")),
                () -> String.valueOf(number));
    }

    private String orIfEmpty(String result, Supplier<String> alternative) {
        return result.isEmpty() ? alternative.get() : result;
    }

    private class Rule {
        private final Predicate<Integer> applies;
        private final String result;

        Rule(Predicate<Integer> applies, String result) {
            this.applies = applies;
            this.result = result;
        }

        String getResult() {
            return result;
        }

        boolean appliesTo(int number) {
            return applies.test(number);
        }
    }
}
