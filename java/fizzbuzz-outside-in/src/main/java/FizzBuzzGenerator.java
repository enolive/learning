import java.util.Arrays;

public class FizzBuzzGenerator {
    private final Rule[] chainOfRules;
    private final Rule[] defaultRules = new Rule[]{
            new FizzBuzzRule("Fizz-Buzz", 3, 5),
            new FizzBuzzRule("Buzz", 5),
            new FizzBuzzRule("Fizz", 3)
    };

    public FizzBuzzGenerator(Rule... chainOfRules) {
        this.chainOfRules = (chainOfRules.length != 0)
                ? chainOfRules
                : defaultRules;
    }

    public String calculate(int input) {
        return Arrays.stream(chainOfRules)
                     .filter(r -> r.appliesTo(input))
                     .map(r -> r.getResult())
                     .findFirst()
                     .orElse(String.valueOf(input));
    }
}
