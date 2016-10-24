import java.util.Arrays;
import java.util.List;

public class FizzBuzzEngine {

    private final List<Rule> rules = Arrays.asList(
            new Rule(15, "Fizz-Buzz"),
            new Rule(3, "Fizz"),
            new Rule(5, "Buzz"));

    public String calculateNext(final int number) {
        return rules.stream()
                .filter(r -> r.appliesTo(number))
                .map(Rule::getResult)
                .findFirst()
                .orElse(Integer.toString(number));
    }

}
