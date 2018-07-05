class FizzBuzz {
    static private divisibleBy = { int number, int divisor -> number % divisor == 0 }
    static private rules = [
            new Tuple2(divisibleBy.rcurry(3), "Fizz"),
            new Tuple2(divisibleBy.rcurry(5), "Buzz"),
    ]

    def static calculate(int input) {
        def result = rules.findAll { appliesTo, _ -> appliesTo(input) }
                          .collect { _, result -> result }
                          .join('-')
        Optional.of(result)
                .filter { !it.empty }
                .orElseGet { input.toString() }
    }
}
