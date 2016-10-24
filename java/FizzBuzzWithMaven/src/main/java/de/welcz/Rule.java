package de.welcz;

class Rule {
    private final int denominator;
    private final String result;

    Rule(int denominator, String result) {
        this.denominator = denominator;
        this.result = result;
    }

    private static boolean isDivisibleBy(int number, int denominator) {
        return number % denominator == 0;
    }

    String getResult() {
        return result;
    }

    boolean appliesTo(int number) {
        return isDivisibleBy(number, denominator);
    }
}
