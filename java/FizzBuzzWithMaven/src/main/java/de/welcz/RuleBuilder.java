package de.welcz;

/**
 * Fluent interface builder for {@link Rule}
 */
class RuleBuilder {
    private int denominator;
    private String result;

    RuleBuilder forDenominator(int denominator) {
        this.denominator = denominator;
        return this;
    }

    RuleBuilder givingResult(String result) {
        this.result = result;
        return this;
    }

    Rule create() {
        return new Rule(denominator, result);
    }
}