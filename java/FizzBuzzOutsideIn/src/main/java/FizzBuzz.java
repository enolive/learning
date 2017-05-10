import java.util.Arrays;

public class FizzBuzz {
    private final Rule[] rules;

    public FizzBuzz(Rule... rule) {
        this.rules = rule;
    }

    public FizzBuzz() {
        this(new BuzzFizzRule(), new FizzRule(), new BuzzRule());
    }

    public String convert(int number) {
        return Arrays.stream(rules)
                .filter(r -> r.appliesTo(number))
                .map(r -> r.getResult())
                .findFirst()
                .orElse(Integer.toString(number));
    }
}
