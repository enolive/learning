package de.welcz;

import org.jetbrains.annotations.Contract;

/**
 * Defines a fizz-buzz rule.
 * If the rule applies to the number, the result should be given.
 */
class Rule {
    private final int denominator;
    private final String result;

    Rule(int denominator, String result) {
        this.denominator = denominator;
        this.result = result;
    }

    @Contract(pure = true)
    String getResult() {
        return result;
    }

    @Contract(pure = true)
    boolean appliesTo(int number) {
        return isDivisibleBy(number, denominator);
    }

    @Contract(pure = true)
    private static boolean isDivisibleBy(int number, int denominator) {
        return number % denominator == 0;
    }
}
