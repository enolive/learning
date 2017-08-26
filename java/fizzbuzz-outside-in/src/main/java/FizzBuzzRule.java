import java.util.Arrays;

public class FizzBuzzRule implements Rule {
    private final String result;
    private final int[] denominators;

    public FizzBuzzRule(String result, int ...denominators) {
        this.result = result;
        this.denominators = denominators;
    }

    @Override
    public boolean appliesTo(int input) {
        return Arrays.stream(denominators).allMatch(d -> isDivisibleBy(input, d));
    }

    @Override
    public String getResult() {
        return result;
    }

    private boolean isDivisibleBy(int input, int denominator) {
        return input != 0 && input % denominator == 0;
    }
}
