public class Rule {
    private final int denominator;
    private final String result;

    public Rule(int denominator, String result) {
        this.denominator = denominator;
        this.result = result;
    }

    private static boolean isDivisibleBy(int number, int denominator) {
        return number % denominator == 0;
    }

    public String getResult() {
        return result;
    }

    public boolean appliesTo(int number) {
        return isDivisibleBy(number, denominator);
    }
}
