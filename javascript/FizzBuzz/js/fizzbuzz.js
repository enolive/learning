class FizzBuzzEngine {
    calculateNext(number) {
        var rules = [
            {divider: 15, result: "Fizz-Buzz"},
            {divider: 3, result: "Fizz"},
            {divider: 5, result: "Buzz"}
        ];

        var ruleFound = rules
            .find((rule) => FizzBuzzEngine.isDivisibleBy(number, rule.divider));

        return ruleFound != null
            ? ruleFound.result
            : number.toString();
    }

    static isDivisibleBy(number, divider) {
        return number % divider == 0;
    }
}