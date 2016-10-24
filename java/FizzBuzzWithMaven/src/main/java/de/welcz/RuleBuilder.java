package de.welcz;

class RuleBuilder {
    private int denominator;
    private String result;

    RuleBuilder denominator(int denominator) {
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