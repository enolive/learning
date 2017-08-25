public class FizzBuzzGenerator {
    public String calculate(int input) {
        if (isDivisibleBy(input, 3) && isDivisibleBy(input, 5)) {
            return "Buzz";
        }

        if (isDivisibleBy(input, 5)) {
            return "Buzz";
        }
        
        if (isDivisibleBy(input, 3)) {
            return "Fizz";
        }
        
        return String.valueOf(input);
    }

    private boolean isDivisibleBy(int input, int denominator) {
        return input % denominator == 0;
    }
}
