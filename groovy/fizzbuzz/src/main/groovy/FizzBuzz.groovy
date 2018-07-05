class FizzBuzz {
    def static calculate(int input) {
        def rules = [
                new Tuple2<Integer, String>(3, "Fizz"),
                new Tuple2<Integer, String>(5, "Buzz"),
        ]
        def result = rules.findAll { isDivisibleBy input, it.first }
                .collect { it.second }
                .join('-')
        if (result.empty) input.toString() else result
    }

    private static isDivisibleBy(int input, int divisor) {
        input % divisor == 0
    }
}
