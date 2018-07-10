class Generator {
    private _rules = [
        new Rule('Fizz-Buzz', 3, 5),
        new Rule('Fizz', 3),
        new Rule('Buzz', 5),
    ];

    resultFor(number: number): string {
        let [matchingRule] = this._rules
            .filter(r => r.appliesTo(number))
            .map(r => r.result);
        return matchingRule || number.toString();
    }
}
